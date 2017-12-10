#r @"D:\packages\FSharp.Data\lib\net45\FSharp.Data.dll"
#load "Parsing.fs"
#load "Result.fs"

open System
open FSharp.Data

module Lego = 
    open Parsing
    
    type [<Struct>] Reference = Reference of int

    let tryReference = function Regex "([0-9]{4,6})" [Int ref] -> ref |> Reference |> Some | _ -> None

module Bid = 
    type Bid = 
        { Date:DateTime
          Url:string 
          Title:string
          Description:string
          Price: decimal option
          City:string
          ZipCode: int }
    let title b = b.Title
    let desc b = b.Description
    let url b = b.Url

open Bid
open Result
open Parsing

module Pricing = 
    type Pricing = 
        { Min:decimal
          Max:decimal }
    let pricing prices = 
        if prices |> Array.isEmpty then None
        else Some { Min = prices |> Array.min; Max = prices |> Array.max } 

let (<|>) f g x = 
    (f <|> g) x
    |> Result.mapError (fun (e1,e2) -> [e1; e2])

let (>>.) f g x = f x |> Result.bind(fun _ -> g x)

module BrickLink = 
    open Lego
    open Pricing
    open Parsing
    open FSharp.Data

    type Catalog = JsonProvider< "bricklink_catalog.json" >

    type [<Struct>] Parts = Parts of int
    type Specs = 
        { Reference : Reference
          Parts : Parts
          Pricing : Pricing }
    
    let tryParts = function
        | Regex "([0-9]+)\s*Parts" [Int r] -> r |> Parts |> Some
        | _ -> None
    
    type SpecError = 
        | PartsNotFound of Reference
        | PricingNotFound of Reference

    let infos (Reference r)= 
        let e = HtmlDocument.Load(sprintf "https://www.bricklink.com/v2/catalog/catalogitem.page?S=%i" r)
        let partsR = 
            e.CssSelect("a.links")
            |> List.choose (fun x -> x.DirectInnerText() |> tryParts)
            |> List.tryHead
            |> Result.ofOption (Reference r |> PartsNotFound)

        let pricingR = 
            let price = function
                | Regex "EUR\s*([0-9]+.[0-9])" [Currency p] -> Some p
                | _ -> None
            e.CssSelect("#_idAddToWantedLink")
            |> List.choose (fun x -> x.TryGetAttribute("data-itemid") |> Option.map (fun y -> y.Value()))
            |> List.tryHead
            |> Option.map(sprintf "https://www.bricklink.com/ajax/clone/catalogifs.ajax?itemid=%s&iconly=0" >> Http.RequestString)
            |> Option.map Catalog.Parse
            |> Option.bind(fun response -> 
                response.List
                |> Array.choose (fun x -> price x.MDisplaySalePrice)
                |> Pricing.pricing)
            |> Result.ofOption (Reference r |> PricingNotFound)

        partsR
        |> Result.bind (fun parts -> 
            pricingR |> Result.map (fun pricing -> 
                { Reference = Reference r
                  Pricing = pricing
                  Parts = parts }))

module Analyzing = 
    type AnalyzingError = 
        | EmptyBox of Bid
        | ManualOnly of Bid
        | ReferenceNotFound of Bid
    
    let private empty b = 
        if b |> Bid.title |> Parsing.icontains "vide" then Error (EmptyBox b)
        else Ok ()

    let private lego b = 
        b 
        |> Bid.title
        |> Lego.tryReference
        |> Option.orElseWith (fun () -> b |> Bid.desc |> Lego.tryReference)
        |> Result.ofOption (ReferenceNotFound b)

    let legos b = 
        let legoPlusPrice = function
            | Regex "([0-9]{4,6})[^0-9]*([0-9\.,]*)\s*[euro|€]" [Int r; Currency p] -> Some (Lego.Reference r, p)
            | _ -> None

        empty b
        |> Result.bind(fun _ -> 
            match b |> Bid.desc |> Parsing.split ["\n";Environment.NewLine] |> Array.choose legoPlusPrice with
            | [||] | [|_|] -> lego b |> Result.map(fun x -> Array.singleton (x,b))
            | bids -> bids |> Array.map (fun (r,p) -> r, { b with Price = Some p }) |> Ok)

module Cache = 
    open System.Collections.Generic
    open System.Collections.Concurrent
    
    type Cache<'a, 'b>  = private Cache of IDictionary<'a,'b>

    let create () = new ConcurrentDictionary<_,_>() :> IDictionary<_,_> |> Cache

    let cache (Cache c) f = 
        fun x ->
            match c.TryGetValue(x) with
            | (true, y) -> y
            | _ -> 
                let y = f x
                c.Add(x, y)
                y

module Leboncoin = 
    type LbcSearchRq = JsonProvider< "lbc_search_request.json", RootName = "LbcRq", SampleIsList=true >
    type LbcSearchRs = JsonProvider< "lbc_search_response.json", RootName = "LbcRs", SampleIsList=true >

    type rq = LbcSearchRq

    type Region = 
        | IleOfFrance

    type Location = 
        | Anywhere
        | ZipCode of int
        | Department of int
        | Region of Region

    type Category = 
        | Anything
        | Property

    type Keywords = 
        | Everything
        | Keywords of string list

    let request category location keywords pivot = 
        
        let (zipCodes, department, region) = 
            match location with
            | Anywhere -> Array.empty, None, None
            | ZipCode x -> [|rq.CityZipcode("", string x)|], None, None
            | Department x -> Array.empty, Some (string x), None
            | Region IleOfFrance -> Array.empty, None, Some "12"
        
        let cat = 
            match category with
            | Anything -> None
            | Property -> Some "9"
        
        let kw = 
            match keywords with
            | Everything -> None
            | Keywords k -> k |> String.concat " " |> Some 

        let filters = 
            rq.Filters(
                rq.Category(cat),
                rq.Enums([|"offer"|]),
                rq.Keywords(kw, Some "all"),
                rq.Location(
                    rq.Area(0m, 0m, 0m),
                    zipCodes,
                    region, 
                    false, 
                    false,
                    department),
                    rq.Owner(), 
                    rq.Owner())
        rq.LbcRq("desc", filters, "all", "time", pivot, 20, 2)
        
    let post (rq:string -> rq.LbcRq) = 
        let rec post pivot = 
            seq {
                printfn "%s" pivot
                let response = 
                    (Http.RequestString(
                        "https://api.leboncoin.fr/api/parrot/v1/search",
                        httpMethod = "POST",
                        headers = 
                            [ "User-Agent", "LBC;Android;4.4.2;GT-N7100;phone;22239501286c360d;wifi;4.2.5.0;42500;1"
                              "api_key", "ba0c2dad52b3ec"
                              "Content-Type", "application/json; charset=UTF-8"
                              "Accept-Encoding", "gzip" ],
                        body = HttpRequestBody.TextRequest ((rq pivot).JsonValue.ToString()))
                     |> LbcSearchRs.Parse)
                yield! 
                    response.Ads 
                    |> Array.map (fun x -> 
                        { Date = x.IndexDate
                          Url = x.Url
                          Price = x.Price |> Array.tryHead |> Option.map decimal
                          Title = x.Subject
                          Description = x.Body
                          City = x.Location.City
                          ZipCode = x.Location.Zipcode })
                match response.Pivot with
                | Some p -> yield! post p
                | None -> () }
        post "0,0,0"

fsi.AddPrinter<DateTime>(fun x -> x.ToString("O"))
fsi.AddPrinter<Bid> (fun b -> sprintf "[%s] %s - %s @ %s / %i - %s" (b.Date.ToString("yyyy-MM-dd")) (match b.Price with Some p -> sprintf "%.2f€" p | None -> "-") b.Title b.Url b.ZipCode b.City)

open BrickLink
fsi.AddPrinter<Specs>(fun s -> 
    let (Parts parts) = s.Parts 
    let (Lego.Reference ref) = s.Reference
    sprintf "[%.2f-%.2f€] #%i %i parts" s.Pricing.Min s.Pricing.Max ref parts )
open Leboncoin

let infos = 
    let cache = Cache.create ()
    let f = Cache.cache cache BrickLink.infos
    fun x ->
        printfn "%A" x
        f x

type SearchError = 
    | AnalyzingError of Analyzing.AnalyzingError
    | SpecError of BrickLink.SpecError * Bid
    | PricingNotFound of Bid

let all = 
    request Anything (Department 77) (Keywords ["Lego technic"]) 
    |> post
    |> Seq.collect (Analyzing.legos >> Result.toSeq >> Seq.map (Result.mapError AnalyzingError))
    |> Seq.map (Result.bind (fun (r,b) -> r |> infos |> Result.map (fun x -> x,b) |> Result.mapError (fun s -> SpecError (s, b))))
    |> Seq.map (
        Result.bind (fun (s,b) -> 
            b.Price |> Option.map (fun p' -> let (Parts n) = s.Parts in p'/ (decimal n)) 
            |> Result.ofOption (PricingNotFound b)
            |> Result.map (fun r -> r, (s,b))))
    |> Seq.toList

let analyzingError = all |> List.fold (fun s -> function Error (AnalyzingError e) -> e :: s | _ -> s) List.empty
let specError = all |> List.fold (fun s -> function Error (SpecError (e,x)) -> (e,x) :: s | _ -> s) List.empty
let pricingNotFoundError = all |> List.fold (fun s -> function Error (PricingNotFound e) -> e :: s | _ -> s) List.empty

let oks = all |> List.fold (fun s -> function Ok x -> x :: s | _ -> s) List.empty

let day y m d = DateTime(y,m,d, 0, 0, 0, DateTimeKind.Utc)
let d2017 = day 2017
let dec17 = d2017 12
let nov17 = d2017 11
let oct17 = d2017 10

oks 
|> List.sortBy fst
|> List.filter (snd >> fst >> fun x -> x.Parts > BrickLink.Parts 900)
//|> List.filter (snd >> snd >> fun x -> x.Price < Some 150m)
|> List.filter (snd >> snd >> fun x -> x.Date > (dec17 05))
//|> List.groupBy(snd >> fst >> fun x -> x.Reference)
, (oks |> Seq.map (snd >> snd >> fun x -> x.Url) |> set |> Seq.length, all |> List.length)

