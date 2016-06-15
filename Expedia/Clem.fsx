#r """..\packages\FSharp.Data.2.3.0\lib\net40\FSharp.Data.dll"""

open FSharp.Data
open System

type Result<'t, 'f> = 
    | Success of 't
    | Failure of 'f

type HotelId = HotelId of int
type TpId = TpId of int
type PointOfSell = PointOfSell of string
type Day = Day of int

type ErrorMessage = ErrorMessage of string
type Error = Error of HotelId * ErrorMessage

type RegionId = RegionId of int64 //Gaia Id

module ExpediaRest = 
    let load (HotelId hotelId) (response:HttpResponse) f =
        match response.StatusCode, response.Body with
        | 200, HttpResponseBody.Text r -> f (HotelId hotelId) r
        | _, HttpResponseBody.Binary _ -> Failure ( Error (HotelId hotelId, ErrorMessage "Binary content instead text" ))
        | statusCode, HttpResponseBody.Text error -> Failure (Error (HotelId hotelId, ErrorMessage (sprintf "error(%i) %s" statusCode error)))

module TopPointOfSell =  
    type private PointOfSellApi = JsonProvider< """tpos.sample.json""", SampleIsList=true >
    type private TopPointOfSell = TopPointOfSell of TpId * (RegionId list) 
    type private PointOfSellMapping = HtmlProvider< """pointofsell.sample.html""" >

    let load user password (HotelId hotelId) = 
        let response = Http.Request(sprintf "https://services.expediapartnercentral.com/top-tpids/lodgingSort/v1/hops/HopsTopTpidsAndRegions?hotelId=%i" hotelId, headers = [ HttpRequestHeaders.BasicAuth user password ], silentHttpErrors=true)
        
        ExpediaRest.load (HotelId hotelId) response <| fun (HotelId hotelId) response ->
            let result = response |> PointOfSellApi.Parse
            match result.Error with
            | Some error -> Failure (Error (HotelId hotelId, ErrorMessage error))
            | _ -> Success (result.HopsTpidsList |> Array.map(fun h -> TpId h.Tpid, (h.SortedRegionList |> Array.toList |> List.map (int64 >> RegionId))))
            
    let tpidMap = PointOfSellMapping.Load("""pointofsell.sample.html""").Tables.``TPID to Point of Sale Mapping``.Rows |> Array.map(fun r -> (TpId r.TPID), (PointOfSell r.``Point of Sale``)) |> Map.ofArray

module ConversationCountService = 
    type UnreadCount = UnreadCount of int
    type private ConversationCountServiceApi = JsonProvider< """{"unreadCount":0}""" >

    let load user password (HotelId hotelId) = 
        let response = Http.Request(sprintf "https://services.expediapartnercentral.com/conversations/hotels/%i/unreadmessages/count" hotelId, headers = [ HttpRequestHeaders.BasicAuth user password ], silentHttpErrors=true)
        
        ExpediaRest.load (HotelId hotelId) response <| fun (HotelId hotelId) response ->
            (response |> ConversationCountServiceApi.Parse).UnreadCount |> UnreadCount |> Success

module FairShareService = 
    type FairShareServiceApi = JsonProvider<"""fairShare.sample.json""">

    let load user password (HotelId hotelId) (Day day) = 
        let response = Http.Request(sprintf "https://services.expediapartnercentral.com/insights/public/v1/fairShare?hotelId=%i&dayNum=%i" hotelId day, headers = [ HttpRequestHeaders.BasicAuth user password ], silentHttpErrors=true)

        ExpediaRest.load (HotelId hotelId) response <| fun (HotelId hotelId) response ->
            let responseParsed = response|> FairShareServiceApi.Parse 
            match responseParsed.ErrorCode.JsonValue.AsString(), responseParsed.ErrorMsg.JsonValue.AsString() with
            | null, null -> responseParsed |> Success
            | errorCode, errorMsg -> Failure(Error(HotelId hotelId, ErrorMessage(sprintf "%s errorCode: %s" errorCode errorMsg)))

module SortRank = 
    type Price = USD of decimal
    type SearchDate = SearchDate of DateTime
    type CheckinDate = CheckinDate of DateTime
    
    type Stat = 
        { AverageRank : Price //Average sort rank (for all searches done on the specified search date for the specified checkin date on the specified TPID and region).
          AveragePrice : Price //Average price is returned in USD.
          AverageCompensation : Price //Average compensation is returned in USD. TODO : question, because I don't know what it is ?
          CheckinDate : CheckinDate }
    
    type Region = 
        { RegionId : RegionId
          Datas : Stat list } //datas for all days

    type SortRank = 
        { HotelId : HotelId
          SearchDate : SearchDate
          Regions : (TpId * Region) list }

    type private SortRankApi = JsonProvider< """sortrank.sample.json""", SampleIsList=true >
    
    let dateString (date:DateTime) = date.ToString("yyyy-MM-dd")
        
    let load user password (HotelId hotelId) searchDates = 
        //TODO : use search parameter : ?hotelId=1&searchDate=2016-05-15&checkin=2016-05-16&numDays=1
        let searchDatesP = String.Join(",",searchDates |> List.map (fun (SearchDate d) -> dateString d) |> List.toArray)
        let response = Http.Request(sprintf "https://services.expediapartnercentral.com/sort-ranks/lodgingSort/v1/hops/HopsAverageRanks?hotelId=%i&searchDate=%s" hotelId searchDatesP , headers = [ HttpRequestHeaders.BasicAuth user password ], silentHttpErrors=true)
        ExpediaRest.load (HotelId hotelId) response <| fun (HotelId hotelId) response ->
            let parsed = response |> SortRankApi.Parse
            
            let data (d:SortRankApi.Datum) = 
                { AverageRank = d.AvgRank |> USD
                  AveragePrice = d.AvgPrice |> USD
                  AverageCompensation = d.AvgComp |> USD
                  CheckinDate = CheckinDate d.CheckinDate }

            let tpid (t:SortRankApi.Tpid) = 
                t.Regions
                |> Array.map(fun r -> 
                    t.Tpid |> TpId,
                    { RegionId = r.RegionId |> int64 |> RegionId
                      Datas = 
                        r.Data
                        |> Array.map data
                        |> Array.toList })
            match parsed.Error with
            | Some error -> Error(HotelId hotelId, ErrorMessage error) |> Failure
            | None ->
                parsed.SearchDates
                |> Array.map (fun sd ->
                    { HotelId = HotelId hotelId
                      SearchDate = SearchDate sd.SearchDate
                      Regions = sd.Tpids |> Array.collect tpid |> Array.toList })
                |> Success

module CompetitorSetEventsService = 
    type StartDate = StartDate of DateTime
    type EndDate = EndDate of DateTime

    let private dateToString (date:DateTime) = date.ToString("yyyy-MM-dd")
        
    type CompetitorSetEventsApi = JsonProvider< """competitorsetecents.sample.json""" >
    
    let load user password (HotelId hotelId) (StartDate startDate) (EndDate endDate) = 
        let response = Http.Request(sprintf "https://services.expediapartnercentral.com/insights/public/v1/addCompSet?hotelId=%i&startDate=%s&endDate=%s" hotelId (dateToString startDate) (dateToString endDate), headers = [ HttpRequestHeaders.BasicAuth user password ], silentHttpErrors=true)
        ExpediaRest.load (HotelId hotelId) response <| fun (HotelId hotelId) response ->
            let responseParsed = response|> CompetitorSetEventsApi.Parse 
            match String.IsNullOrEmpty(responseParsed.ErrorCode.JsonValue.AsString()), String.IsNullOrEmpty(responseParsed.ErrorMsg.JsonValue.AsString()) with
            | true, true -> responseParsed |> Success
            | errorCode, errorMsg -> Failure(Error(HotelId hotelId, ErrorMessage(sprintf "%s errorCode: %s" (responseParsed.ErrorCode.JsonValue.AsString()) (responseParsed.ErrorMsg.JsonValue.AsString()))))
        

open CompetitorSetEventsService
open SortRank

let jun16 d = DateTime(2016,6,d,0,0,0,DateTimeKind.Utc) 

CompetitorSetEventsService.load "EQC15240057test" "mVtM7Uq3" (HotelId 15240057) (jun16 1 |> StartDate) (jun16 3 |> EndDate)
//14 -> 16
SortRank.load "EQC15240057test" "mVtM7Uq3" (HotelId 15240057) [(14 |> jun16 |> SearchDate)]

FairShareService.load "EQC15240057test" "mVtM7Uq3" (HotelId 15240057) (Day 2)
TopPointOfSell.load "EQC15240057test" "mVtM7Uq3" (HotelId 15240057)
TopPointOfSell.tpidMap
ConversationCountService.load "EQC15240057test" "mVtM7Uq3" (HotelId 15240057)