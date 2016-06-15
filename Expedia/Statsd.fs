module Statsd

open System

let client = 
    try
        new System.Net.Sockets.UdpClient("192.168.137.12", 8125) |> Some
    with ex -> 
        printfn "%O" ex
        None

let private send format args = 
    let text = System.String.Format(System.Globalization.CultureInfo.InvariantCulture, format, args |> List.toArray).ToLowerInvariant()
    printfn "%s" text
    let bytes = System.Text.Encoding.ASCII.GetBytes(text)
    client |> Option.map (fun c -> printfn "sent"; c.Send(bytes, bytes.Length) |> ignore) |> ignore

let timer (name:string) (timeSpan:TimeSpan) = send "{0}:{1:d}|ms" [name; timeSpan.TotalMilliseconds]

let nullTimer (name:string) (timeSpan:TimeSpan) = printfn "timer -> %s:%.2fms" name timeSpan.TotalMilliseconds

let counter (name:string) = send "{0}:1|c" [name]

let nullCounter (name:string) = printfn "counter -> %s" name