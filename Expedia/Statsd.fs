module Statsd

open System

let client = 
    try
        new System.Net.Sockets.UdpClient("statsd.connectivity-services", 8125) |> Some
    with ex -> 
        printfn "%O" ex
        None

let private send format args = 
    let text = System.String.Format(System.Globalization.CultureInfo.InvariantCulture, format, args |> List.toArray).ToLowerInvariant()
    let bytes = System.Text.Encoding.ASCII.GetBytes(text)
    client |> Option.map (fun c -> c.Send(bytes, bytes.Length) |> ignore) |> ignore

let timer (name:string) (timeSpan:TimeSpan) = send "{0}:{1:d}|ms" [name; timeSpan.TotalMilliseconds]

let counter (name:string) = send "{0}:1|c" [name]