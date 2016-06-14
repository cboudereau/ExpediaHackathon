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
type UnreadCount = UnreadCount of int

module ExpediaRest = 
    let load (HotelId hotelId) (response:HttpResponse) f =
        match response.StatusCode, response.Body with
        | 200, HttpResponseBody.Text r -> f (HotelId hotelId) r
        | _, HttpResponseBody.Binary _ -> Failure ( Error (HotelId hotelId, ErrorMessage "Binary content instead text" ))
        | statusCode, HttpResponseBody.Text error -> Failure (Error (HotelId hotelId, ErrorMessage (sprintf "error(%i) %s" statusCode error)))

module TopPointOfSell =  
    type private TopPointOfSell = TopPointOfSell of TpId * (GaiaId list) 

    let load user password (HotelId hotelId) = 
        let response = Http.Request(sprintf "https://services.expediapartnercentral.com/top-tpids/lodgingSort/v1/hops/HopsTopTpidsAndRegions?hotelId=%i" hotelId, headers = [ HttpRequestHeaders.BasicAuth user password ], silentHttpErrors=true)
        
        ExpediaRest.load (HotelId hotelId) response <| fun (HotelId hotelId) response ->
            let result = response |> PointOfSellApi.Parse
            match result.Error with
            | Some error -> Failure (Error (HotelId hotelId, ErrorMessage error))
            | _ -> Success (result.HopsTpidsList |> Array.map(fun h -> TpId h.Tpid, (h.SortedRegionList |> Array.toList |> List.map (int64 >> GaiaId))))
            
                        
    let tpidMap = PointOfSellMapping.Load("""pointofsell.sample.html""").Tables.``TPID to Point of Sale Mapping``.Rows |> Array.map(fun r -> (TpId r.TPID), (PointOfSell r.``Point of Sale``)) |> Map.ofArray

module ConversationCountService = 
    type private ConversationCountServiceApi = JsonProvider< """{"unreadCount":0}""" >

    let load user password (HotelId hotelId) = 
        let response = Http.Request(sprintf "https://services.expediapartnercentral.com/conversations/hotels/%i/unreadmessages/count" hotelId, headers = [ HttpRequestHeaders.BasicAuth user password ], silentHttpErrors=true)
        
        ExpediaRest.load (HotelId hotelId) response <| fun (HotelId hotelId) response ->
            (response |> ConversationCountServiceApi.Parse).UnreadCount |> UnreadCount |> Success

TopPointOfSell.load "EQC15240057test" "mVtM7Uq3" (HotelId 15240057)
ConversationCountService.load "EQC15240057test" "mVtM7Uq3" (HotelId 15240057)