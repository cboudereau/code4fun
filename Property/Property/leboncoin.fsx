#r "../packages/FSharp.Data/lib/net45/FSharp.Data.dll"
#load "Parsing.fs"
#load "Result.fs"

open System
open FSharp.Data

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

open Leboncoin

request Property (ZipCode 77210) (Everything) |> post |> Seq.toList