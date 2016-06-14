#r """..\packages\FSharp.Data.2.3.0\lib\net40\FSharp.Data.dll"""
open FSharp.Data

type Result<'t, 'f> = 
    | Success of 't
    | Failure of 'f

type PointOfSellMapping = HtmlProvider< """pointofsell.sample.html""" >

type PointOfSellApi = JsonProvider< """tpos.sample.json""", SampleIsList=true >

type HotelId = HotelId of int
type TpId = TpId of int
type PointOfSell = PointOfSell of string

type ErrorMessage = ErrorMessage of string
type Error = Error of HotelId * ErrorMessage

type GaiaId = GaiaId of int64

type TopPointOfSell = TopPointOfSell of TpId * (GaiaId list) 

let load user password (HotelId hotelId) = 
    let response = Http.Request(sprintf "https://services.expediapartnercentral.com/top-tpids/lodgingSort/v1/hops/HopsTopTpidsAndRegions?hotelId=%i" hotelId, headers = [ HttpRequestHeaders.BasicAuth user password ], silentHttpErrors=true)
    
    match response.StatusCode, response.Body with
    | 200, HttpResponseBody.Text text ->  
        let result = text |> PointOfSellApi.Parse
        match result.Error with
        | Some error -> Failure (Error (HotelId hotelId, ErrorMessage error))
        | _ -> Success (result.HopsTpidsList |> Array.map(fun h -> TpId h.Tpid, (h.SortedRegionList |> Array.toList |> List.map (int64 >> GaiaId))))
    | 200, HttpResponseBody.Binary _ ->
        Failure ( Error (HotelId hotelId, ErrorMessage "Binary content instead text" ))
    | statusCode, _ -> Failure (Error (HotelId hotelId, ErrorMessage (sprintf "error %i" statusCode)))

load "EQC15240057test" "mVtM7Uq3" (HotelId 15240057)


let pointOfSellPage = PointOfSellMapping.Load("""pointofsell.sample.html""")
    //PointOfSellHtml.Load("""https://developer.expediapartnercentral.com/apis/expedia-partner-hackathon/top-point-of-sale-and-regions-api/reference.html""")



let tpidMap = pointOfSellPage.Tables.``TPID to Point of Sale Mapping``.Rows |> Array.map(fun r -> (TpId r.TPID), (PointOfSell r.``Point of Sale``)) |> Map.ofArray



