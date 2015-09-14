type Xml = 
    | Zero
    | Content of string
    with
        static member combine f first second = 
            match first, second with
            | Zero, _ -> second
            | _, Zero -> first
            | Content first, Content second -> Content (f first second)

let xmls = [ "1"; "2"; "3" ]

let contents = xmls |> Seq.map Content

let sum = contents |> Seq.fold (fun state i -> Xml.combine (+) state i) Xml.Zero

let sumBack = Xml.Zero |> Seq.foldBack (fun i state -> Xml.combine (+) state i) contents