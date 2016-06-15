module String
open System
open System.Text.RegularExpressions

let icontains search source = (source:string).IndexOf(search, StringComparison.InvariantCultureIgnoreCase) <> -1

let (|Contains|_|) search source =
    if icontains search source then Some source
    else None 

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern, RegexOptions.CultureInvariant ||| RegexOptions.IgnoreCase)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let (|Time|_|) source = 
    match source with
    | Regex "([0-9]+)(?:mn|m)" [i] -> TimeSpan.FromMinutes(float i) |> Some
    | Regex "([0-9]+)(?:h)" [i] -> TimeSpan.FromHours(float i) |> Some
    | Regex "([0-9]+)(?:s)" [i] -> TimeSpan.FromSeconds(float i) |> Some
    | _ -> None
