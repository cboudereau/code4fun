
#r @"..\packages\ExcelProvider.0.1.2\lib\net40\ExcelProvider.dll"

open FSharp.ExcelProvider

type Rate = ExcelFile<"excel.xlsx", "A1:B2">
type Prices = ExcelFile<"excel.xlsx", "A4:B5">

let rateFile = Rate()

let row = rateFile.Data |> Seq.head

let priceFile = Prices()

let priceRow = priceFile.Data |> Seq.head


#r @"..\packages\FSharp.Data.2.2.5\lib\net40\FSharp.Data.dll"

#r "System.Xml.Linq"

open FSharp.Data

type PersonXml = XmlProvider<"<root><firstName>Clem</firstName><name>boud</name></root>">

let xml = PersonXml.Root("Sergii", "Salata").XElement.ToString()


let person = PersonXml.Parse("<root><firstName>Clem</firstName><name>Boudereau</name></root>")


type Person = XmlProvider<"personSample.xml", SampleIsList=true>

let p = Person.Parse("<root><firstName>clem</firstName></root>")

let name = p.Name

let realName = 
    match name with
    | None -> sprintf "%s" "empty name"
    | Some value -> sprintf "%s" value

realName

type Price = 
    | Closed
    | Open of int

let closed = Closed
let pr = Open 10

let viewPrice = 
    match pr with
    | Closed -> 0
    | Open p -> p
