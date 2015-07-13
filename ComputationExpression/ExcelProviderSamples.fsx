
#r @"..\packages\ExcelProvider.0.1.2\lib\net40\ExcelProvider.dll"

open FSharp.ExcelProvider

type Rate = ExcelFile<"excel.xlsx", "A1:B2">
type Prices = ExcelFile<"excel.xlsx", "A4:B5">

let rateFile = Rate()

let row = rateFile.Data |> Seq.head

let priceFile = Prices()

let priceRow = priceFile.Data |> Seq.head