#r @"D:\packages\FSharp.Data\lib\net45\FSharp.Data.dll"

open System
fsi.AddPrinter<DateTime>(fun x -> x.ToString("O"))
open FSharp.Data

type LbcSearchRq = JsonProvider< "lbc_search_request.json", RootName = "LbcRq" >
type LbcSearchRs = JsonProvider< "lbc_search_response.json" >

type rq = LbcSearchRq

type City = City of string
type ZipCode = ZipCode of int

let request (City city, ZipCode zipCode) = 
    let filters = 
        rq.Filters(
            rq.Category(string 9), 
            rq.Enums([|"offer"|]), 
            rq.Keywords(),
            rq.Location(
                rq.Area(0m, 0m, 0m), 
                [| rq.CityZipcode(city, string zipCode) |], 
                string 12, 
                false, 
                false), 
                rq.Keywords(), 
                rq.Keywords())
    rq.LbcRq("desc", filters, "all", "time", "0,0,0", 20, 2)
    
let post (rq:rq.LbcRq) = 
    (Http.RequestString(
        "https://api.leboncoin.fr/api/parrot/v1/search",
        httpMethod = "POST",
        headers = 
            [ "User-Agent", "LBC;Android;4.4.2;GT-N7100;phone;22239501286c360d;wifi;4.2.5.0;42500;1"
              "api_key", "ba0c2dad52b3ec"
              "Content-Type", "application/json; charset=UTF-8"
              "Accept-Encoding", "gzip" ],
        body = HttpRequestBody.TextRequest (rq.JsonValue.ToString()))
     |> LbcSearchRs.Parse).Ads
     |> Array.map (fun x -> x.IndexDate, x.Price |> Array.tryHead, x.Subject)

request (City "Samoreau", ZipCode 77210) |> post