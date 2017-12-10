#r "../packages/FSharp.Data/lib/net45/FSharp.Data.dll"
#load "Leboncoin.fs"

open System
open Leboncoin

fsi.AddPrinter<DateTime>(fun x -> x.ToString("O"))
fsi.AddPrinter<Bid> (fun b -> sprintf "[%s] %s - %s @ %s / %i - %s" (b.Date.ToString("yyyy-MM-dd")) (match b.Price with Some p -> sprintf "%.2f€" p | None -> "-") b.Title b.Url b.ZipCode b.City)

Api.request Property (ZipCode 77210) (Everything) |> Api.post |> Seq.toList