#r """..\packages\FSharp.Data.2.3.0\lib\net40\FSharp.Data.dll"""

#load "String.fs"
#load "Slack.fs"

open FSharp.Data
open System

type Result<'t, 'f> = 
    | Success of 't
    | Failure of 'f

let map f x = 
    match x with
    | Success x' -> f x' |> Success
    | Failure e -> Failure e

type HotelId = HotelId of int
type TpId = TpId of int
type PointOfSell = PointOfSell of string
type Day = Day of int

type ErrorMessage = ErrorMessage of string
type Error = Error of HotelId * ErrorMessage

type RegionId = RegionId of int64 //Gaia Id

let dateString (date:DateTime) = date.ToString("yyyy-MM-dd")

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
    type RoomCount = RoomCount of int
    type FairShare = FairShare of decimal
    type BookedRoom = BookedRoom of int
    type Date = Date of DateTime

    type Daily = 
        { BookedRooms : BookedRoom
          CompSetBookedRooms : BookedRoom
          Date : Date}

    type FairShareData = 
        { CompSetRoomCount : RoomCount
          FairShare : FairShare
          RoomCount : RoomCount
          Daily : Daily list
          }

    let load user password (HotelId hotelId) (Day day) = 
        let response = Http.Request(sprintf "https://services.expediapartnercentral.com/insights/public/v1/fairShare?hotelId=%i&dayNum=%i" hotelId day, headers = [ HttpRequestHeaders.BasicAuth user password ], silentHttpErrors=true)

        ExpediaRest.load (HotelId hotelId) response <| fun (HotelId hotelId) response ->
            let responseParsed = response|> FairShareServiceApi.Parse 

            let daily (d:FairShareServiceApi.Daily) = 
                { BookedRooms = BookedRoom d.BookedRooms
                  CompSetBookedRooms = BookedRoom d.CompSetBookedRooms
                  Date = Date d.Date }

            let data (d:FairShareServiceApi.Data) =
                { CompSetRoomCount = RoomCount d.CompSetRoomCount
                  FairShare = FairShare d.Fairshare
                  RoomCount = RoomCount d.RoomCount
                  Daily = 
                  d.Daily
                  |> Array.map daily
                  |> Array.toList }
            match responseParsed.ErrorCode.JsonValue.AsString(), responseParsed.ErrorMsg.JsonValue.AsString() with
            | null, null -> responseParsed.Data|> data |> Success
            | errorCode, errorMsg -> Failure(Error(HotelId hotelId, ErrorMessage(sprintf "%s errorCode: %s" errorCode errorMsg)))

module SortRank = 
    type private SortRankApi = JsonProvider< """sortrank.sample.json""", SampleIsList=true >

    type Price = USD of decimal
    type SearchDate = SearchDate of DateTime
    type CheckinDate = CheckinDate of DateTime
    type Rank = Rank of decimal
    type Stat = 
        { AverageRank : Rank //Average sort rank (for all searches done on the specified search date for the specified checkin date on the specified TPID and region).
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
    
    let load user password (HotelId hotelId) searchDate = 
        //TODO : use search parameter : ?hotelId=1&searchDate=2016-05-15&checkin=2016-05-16&numDays=1
        let response = Http.Request(sprintf "https://services.expediapartnercentral.com/sort-ranks/lodgingSort/v1/hops/HopsAverageRanks?hotelId=%i&searchDate=%s" hotelId searchDate , headers = [ HttpRequestHeaders.BasicAuth user password ], silentHttpErrors=true)
        ExpediaRest.load (HotelId hotelId) response <| fun (HotelId hotelId) response ->
            let parsed = response |> SortRankApi.Parse
            
            let data (d:SortRankApi.Datum) = 
                { AverageRank = d.AvgRank |> Rank
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
                |> Array.toList
                |> Success

    type SortRankStat = (SearchDate * (PointOfSell * Stat))    

    let stat user password (HotelId hotelId) sd = 
        let convert l : SortRankStat list =  l |> List.collect(fun s -> s.Regions |> List.collect(fun (tpid,r) -> r.Datas |> List.map(fun d -> s.SearchDate, (TopPointOfSell.tpidMap |> Map.find tpid, d))))
        (load user password (HotelId hotelId) sd)
        |> map convert

module InfluxDb =
    open SortRank
    let formatStat (srl:SortRankStat list) = 
        let value = 
            srl
            |> List.toArray
            |> Array.collect(fun (x, (PointOfSell y, z)) -> 
                let (Rank rank) = z.AverageRank
                let (USD price) = z.AveragePrice
                let (USD comp) = z.AverageCompensation
                [|sprintf "rank,pointofsell='%s' value=%.2f" y rank
                  sprintf "price,pointofsell='%s' value=%.2f" y price
                  sprintf "comp,pointofsell='%s' value=%.2f" y comp|])
        String.Join("\r\n", value)

    let send data =
        Http.Request("http://localhost:8086", httpMethod = HttpMethod.Post, body = HttpRequestBody.TextRequest data, silentHttpErrors = true) |> ignore

module CompetitorSetEventsService = 
    type CompetitorSetEventsApi = JsonProvider< """competitorsetecents.sample.json""" >
    type StartDate = StartDate of DateTime
    type EndDate = EndDate of DateTime
    type HotelName = HotelName of string
    type Limit = Limit of int
    type EventDate = EventDate of DateTime

    type CompSet = 
        { EventDate : EventDate
          HotelId : HotelId
          HotelName : HotelName }

    type CompetitorSetEvent = 
        { HotelId : HotelId
          HotelName : HotelName
          EndDate : EndDate
          Limit : Limit
          StartDate : StartDate
          CompSet : CompSet list }
        
    let urlFormat (HotelId hotelId) (StartDate startDate) (EndDate endDate) = 
        sprintf "https://services.expediapartnercentral.com/insights/public/v1/addCompSet?hotelId=%i&startDate=%s&endDate=%s" hotelId (dateString startDate) (dateString endDate)
    
    let load user password (HotelId hotelId) (StartDate startDate) (EndDate endDate) = 
        let response = Http.Request(urlFormat (HotelId hotelId) (StartDate startDate) (EndDate endDate), headers = [ HttpRequestHeaders.BasicAuth user password ], silentHttpErrors=true)
        ExpediaRest.load (HotelId hotelId) response <| fun (HotelId hotelId) response ->
            let responseParsed = response|> CompetitorSetEventsApi.Parse 

            let compSet (cs:CompetitorSetEventsApi.CompSet)= 
               { EventDate = EventDate cs.EventDate
                 HotelId = HotelId cs.HotelId
                 HotelName = HotelName cs.HotelName }

            let data (d:CompetitorSetEventsApi.Data) = 
                { HotelId = HotelId d.HotelId
                  HotelName = HotelName d.HotelName
                  EndDate = EndDate d.EndDate
                  Limit = Limit d.Limit
                  StartDate = StartDate d.StartDate
                  CompSet = 
                    d.CompSet 
                    |> Array.map compSet
                    |> Array.toList }

            match String.IsNullOrEmpty(responseParsed.ErrorCode.JsonValue.AsString()), String.IsNullOrEmpty(responseParsed.ErrorMsg.JsonValue.AsString()) with
            | true, true -> responseParsed.Data |> data |> Success
            | errorCode, errorMsg -> Failure(Error(HotelId hotelId, ErrorMessage(sprintf "%s errorCode: %s" (responseParsed.ErrorCode.JsonValue.AsString()) (responseParsed.ErrorMsg.JsonValue.AsString()))))
        

open CompetitorSetEventsService
open SortRank

let post = Slack.post (Slack.Channel "advise")
let user = "EQC15240057test"
let password = "mVtM7Uq3"
let hotelId = HotelId 15240057

let sendData  user password hotelId sd = SortRank.stat user password hotelId sd |> map (InfluxDb.formatStat >> InfluxDb.send)

open ConversationCountService

let commandParser = 
    async {
        let commandParser (Slack.TeamMember teamMember) (Slack.TextMessage m) = 
            match m with
            | String.Contains "What's up" _ -> 
                let countMessage = 
                    let mc = 
                        match ConversationCountService.load user password hotelId with
                        | Success (UnreadCount i) when i < 2 -> sprintf "%i unread message" i
                        | Success (UnreadCount i) -> sprintf "%i unread messages" i
                        | Failure (Error(_, ErrorMessage em)) -> sprintf "some problem %s" em
                    sprintf "Hello %s, \r\n You have %s" teamMember mc
                let rankMessage = 
                    match SortRank.load user password hotelId "2016-06-14,2016-06-15" with
                    | Success (sortRanks) -> 
                        let datas = sortRanks |> List.collect (fun h -> h.Regions |> List.collect(fun (tpid, r) -> r.Datas))
                        let rank r = let (Rank r) = r.AverageRank in r
                        let min = datas |> List.minBy(fun d -> d.AverageRank) |> rank
                        let max = datas |> List.maxBy(fun d -> d.AverageRank) |> rank
                        sprintf "Well, I see that you have really increased your ranking from %.2f to %.2f since 2016-06-15!" max min
                    | Failure (Error(_, ErrorMessage em)) -> sprintf "I experienced some problems (%s), one moment please..." em
                countMessage + "\r\n" + rankMessage |> post
            
            | String.Contains "What's my ranking today" _ ->
                match SortRank.load user password hotelId "2016-06-15" with
                | Success (sortRanks) -> 
                    let datas = sortRanks |> List.collect (fun h -> h.Regions |> List.collect(fun (tpid, r) -> r.Datas))
                    let rank r = let (Rank r, USD p) = r.AverageRank, r.AveragePrice in r,p
                    let (min, p) = datas |> List.minBy(fun d -> d.AverageRank) |> rank
                    sprintf "You have a good ranking (%.1f at $%.2f)!\r\nGood job !\r\nMay be you want to be the first now :sunglasses:" min p |> post 
                | Failure (Error(_, ErrorMessage em)) -> sprintf "I experienced some problems (%s), one moment please..." em |> post
            
            | String.Contains "What's my ranking yesterday" _ ->
                match SortRank.load user password hotelId "2016-06-14" with
                | Success (sortRanks) -> 
                    let datas = sortRanks |> List.collect (fun h -> h.Regions |> List.collect(fun (tpid, r) -> r.Datas))
                    let rank r = let (Rank r) = r.AverageRank in r
                    let min = datas |> List.minBy(fun d -> d.AverageRank) |> rank
                    let max = datas |> List.maxBy(fun d -> d.AverageRank) |> rank
                    sprintf "I think you are so far between %.1f and %.1f." min max |> post 
                | Failure (Error(_, ErrorMessage em)) -> sprintf "I experienced some problems (%s), one moment please..." em |> post
            
            | String.Contains "What's my best price today" _ ->
                match SortRank.load user password hotelId "2016-06-14" with
                | Success (sortRanks) -> 
                    let datas = sortRanks |> List.collect (fun h -> h.Regions |> List.collect(fun (tpid, r) -> r.Datas |> List.map (fun d -> tpid, d)))
                    let price (tpid, p) = let (USD  pr, CheckinDate cd) = p.AveragePrice,p.CheckinDate in pr, cd, tpid
                    let (minP, minCd, tpid) = datas |> List.minBy(fun (_,d) -> d.AveragePrice) |> price
                    let (PointOfSell pointOfSale) = TopPointOfSell.tpidMap |> Map.find tpid
                    sprintf "A customer stay in your hotel with your best price $%.2f on %s from %s point of sell." minP (dateString minCd) pointOfSale |> post 
                | Failure (Error(_, ErrorMessage em)) -> sprintf "I experienced some problems (%s), one moment please..." em |> post
             
            
            | s -> sprintf "I don't understand : %s" s |> post

        return! Slack.connect (Slack.Bot "expedia") commandParser
    }

//commandParser |> Async.RunSynchronously

fsi.AddPrinter(fun (x : DateTime) -> x.ToString("yyyy-MM-dd"))
let jun16 d = DateTime(2016,6,d,0,0,0,DateTimeKind.Utc) 

CompetitorSetEventsService.load "EQC15240057test" "mVtM7Uq3" (HotelId 15240057) (jun16 1 |> StartDate) (jun16 3 |> EndDate)

//14 -> 16
let sd = jun16 >> SearchDate
let sDate d = dateString (jun16 d) 
let (Success r) = SortRank.load "EQC15240057test" "mVtM7Uq3" (HotelId 15240057) "2016-06-14,2016-06-16"
let datas = r |> List.collect (fun h -> h.Regions |> List.collect(fun (tpid, r) -> r.Datas))
datas |> List.minBy(fun d -> d.AverageRank)
datas |> List.maxBy(fun d -> d.AverageRank)

sendData user password hotelId (sDate 14)

FairShareService.load "EQC15240057test" "mVtM7Uq3" (HotelId 15240057) (Day 2)
TopPointOfSell.load "EQC15240057test" "mVtM7Uq3" (HotelId 15240057)
TopPointOfSell.tpidMap
