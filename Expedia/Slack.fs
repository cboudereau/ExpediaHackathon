module Slack
open System.Net.WebSockets
open System.Threading
open System.IO
open FSharp.Data

type Channel = Channel of string
type Bot = Bot of string
type TeamMember = TeamMember of string
type Email = Email of string
type TextMessage = TextMessage of string

type private SlackUserId = SlackUserId of string
type private RtmStart = JsonProvider<"rtmStart.sample.json">
type private RtmMessage = JsonProvider<"rtmMessage.sample.json", SampleIsList=true>

let [<Literal>] private token = "xoxb-51086347636-RywdMKFiqR8Gf7so4EhAsCNg"

let post (Channel channel) text = 
        Http.Request(
            "https://slack.com/api/chat.postMessage", 
            httpMethod = HttpMethod.Post,
            body = FormValues(
                [ ("token", token)
                  ("channel", channel)
                  ("text", text)
                  ("as_user", "true")
                  ("link_names", "1") ]))
        |> ignore

let private rtmStart = Http.RequestString("https://slack.com/api/rtm.start", query=["token", token], httpMethod="GET") |> RtmStart.Parse

let private tryFindChannel (Channel channel) = rtmStart.Channels |> Array.tryFind(fun c -> c.Name = channel)

let private tryFindUserName (SlackUserId userId) = rtmStart.Users |> Array.tryFind(fun u -> u.Id = userId) |> Option.map(fun u -> TeamMember u.Name)

let private findBotId (Bot bot) = 
    let user = rtmStart.Users |> Array.find(fun u -> u.Name = bot)
    user.Id |> SlackUserId

let private tryFindUserId (Email email) = 
    rtmStart.Users
    |> Array.filter(fun u -> u.Profile.Email = Some email)
    |> Array.tryHead
    |> Option.map(fun u -> SlackUserId u.Id)

let invite channel email =
    match tryFindChannel channel, tryFindUserId email with
    | Some c, Some (SlackUserId userId) -> 
        Http.Request(
            "https://slack.com/api/channels.invite", 
            body = FormValues
                [ ("token", token)
                  ("channel", c.Id)
                  ("user", userId) ],
            httpMethod = HttpMethod.Post)
        |> ignore
    | _ -> ()

open System.Text.RegularExpressions
open System

let tryFindEmail (Email email) = 
    let name = Regex("([^@]+)@.*",RegexOptions.CultureInvariant ||| RegexOptions.IgnoreCase).Replace(email, "$1")
    
    let ocontains search = 
        function
        | Some value -> value |> String.icontains search
        | None -> false

    rtmStart.Users
    |> Array.filter(fun u -> u.Profile.Email |> ocontains name)
    |> Array.map(fun u -> TeamMember u.Name)
    |> Array.tryHead

let private disconnect (webSocket: ClientWebSocket) =
    async {
        try
            do! webSocket.CloseAsync(WebSocketCloseStatus.NormalClosure, "", CancellationToken.None) |> Async.AwaitTask
        with
        | _ -> printfn "slack client shutting down but not gracefully"
    }

let private sendData (webSocket: ClientWebSocket) (data: string) = 
    async {
        let buffer = new ArraySegment<byte>(System.Text.ASCIIEncoding.ASCII.GetBytes(data))
        do! webSocket.SendAsync(buffer, WebSocketMessageType.Text, true, CancellationToken.None) |> Async.AwaitTask
    }

let private ping (webSocket: ClientWebSocket) i =
    async {
        printfn "ping"
        do! sprintf """{"id": %d, "type":"ping"}""" i |> sendData webSocket
    }

let rec private scheduledPing (webSocket: ClientWebSocket) i = 
    async{
        do! Async.Sleep(10000)
        do! i |> ping webSocket
    }

let rec private receive (ms: MemoryStream) (webSocket: ClientWebSocket) = 
    async {
        let buffer = new ArraySegment<byte>(Array.zeroCreate(8192))
        let! res = webSocket.ReceiveAsync(buffer, CancellationToken.None) |> Async.AwaitTask
        ms.Write(buffer.Array, buffer.Offset, res.Count)
        if res.EndOfMessage 
        then
            ms.Seek(0L, SeekOrigin.Begin) |> ignore
            return ms.ToArray() 
        else
            return! webSocket |> receive ms
    }

let rec private readMessage (SlackUserId botId) f (webSocket: ClientWebSocket) =
    async {
        printfn "readMessage for botId:%s" botId
        use ms = new MemoryStream()
        let! data = webSocket |> receive ms
        let message = System.Text.ASCIIEncoding.ASCII.GetString(data) |> RtmMessage.Parse
        match message.Type, message.ReplyTo, message.Text, message.User with
        | String.Contains "pong" _, Some id, _, _ -> 
            printfn "pong"
            return! id + 1 |> ping webSocket
        | String.Contains "message" _, _, Some (String.Contains botId text), Some user when user <> botId -> 
            let userName = 
                match user |> SlackUserId |> tryFindUserName with
                | Some name -> name
                | None -> TeamMember String.Empty
            f userName (TextMessage text)
        | _ -> ()
        return! webSocket |> readMessage (SlackUserId botId) f
    }

let connect bot f = 
    let botId = findBotId bot
    let rec connect () = 
        let webSocket = new ClientWebSocket()
        async {
            try
                do! webSocket.ConnectAsync(new Uri(rtmStart.Url), CancellationToken.None) |> Async.AwaitTask
                readMessage botId f webSocket |> Async.Start
                do! scheduledPing webSocket 0
            with
            | ex -> 
                printfn "%s %s" ex.Message ex.StackTrace
                do! webSocket |> disconnect
                return! connect ()
        }
    connect ()
