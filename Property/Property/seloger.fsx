#r "../packages/FSharp.Data/lib/net45/FSharp.Data.dll"

open FSharp.Data

#r "System.Xml.Linq"

type SeLogerResponse = XmlProvider< "seloger.xml" >

type Kind = 
    | House
    | Appartment

type [<Struct>] InseeCode = InseeCode of int

let get kinds (InseeCode codeInsee) = 
    let kind = function Appartment -> "1" | House -> "2"
    let kinds = "idtypebien", kinds |> Seq.map kind |> String.concat ","
    let buy = "idtt", "2"
    
    Http.RequestString(
        "http://ws.seloger.com/search.xml", 
        [ kinds
          buy
          "ci", string codeInsee ])
    |> SeLogerResponse.Parse
    |> fun x -> x.Annonces
    |> Array.map (fun x -> x.Libelle, x.Longitude, x.Latitude, x.PermaLien)

get [Appartment] (InseeCode 750102) |> Seq.length