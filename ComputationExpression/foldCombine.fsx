
[<Literal>] let xml = "<root></root><!-- <![CDATA[ <previous></previous> ]]> -->" 

#r "System.Xml.Linq"

open System.Xml.Linq

let parsed = System.Xml.Linq.XDocument.Parse(xml)

let comments = System.Linq.Enumerable.OfType<XComment>(parsed.DescendantNodes())
let comment = comments |> Seq.last

type Xml = 
    | Zero
    | Content of string
    with
        static member combine f first second = 
            match first, second with
            | Zero, _ -> second
            | _, Zero -> first
            | Content first, Content second -> 
                printfn "fst: %A; snd %A" first second
                Content (f first second)

let xmls = [ "1"; "2"; "3" ]

let contents = xmls |> Seq.map Content

let sum = contents |> Seq.fold (fun state i -> Xml.combine (+) state i) Xml.Zero

let sumBack = Xml.Zero |> Seq.foldBack (fun i state -> Xml.combine (+) state i) contents