#r "../packages/FSharp.Data/lib/net45/FSharp.Data.dll"

open FSharp.Data

type PapfrSearchResponse = JsonProvider< "papfr_search_response.json" >

let bids = 
    let response = 
        Http.RequestString(
            "https://ws.pap.fr/immobilier/annonces", 
            query = 
                [ "recherche[produit]","vente"
                  "recherche[geo][ids][]", "439"
                  "recherche[geo][ids][]", "37774"
                  "order", "date-desc"
                  "recherche[last_check]", "1511361650"
                  "size", "200"
                  "page", "1" ],
            headers = 
                ["X-Device-Gsf", "38b4f2505d5903c6"
                 "User-Agent", "Dalvik/1.6.0 (Linux; U; Android 4.4.2; GT-N7100 Build/KOT49H)" ]) 
        |> PapfrSearchResponse.Parse
    response.Embedded.Annonce |> Array.map(fun x -> x.Produit, x.Typebien, x.Prix)