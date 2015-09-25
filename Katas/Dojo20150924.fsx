type X = X of int
type Y = Y of int

type Coords = Coords of X * Y
    with
        static member up (Coords (x, (Y y))) = Coords(x, Y (y+1))
        static member down (Coords (x, (Y y))) = Coords(x, Y (y-1))
        static member left (Coords ((X x), y)) = Coords(X (x-1), y)
        static member right (Coords ((X x), y)) = Coords(X (x+1), y)

type Move = 
    | Up
    | Down
    | Left
    | Right
    with
        static member move m c = 
            match m with
            | Up -> Coords.up c
            | Down -> Coords.down c
            | Right -> Coords.right c
            | Left -> Coords.left c

type Element = 
    | Human
    | HumanOnStock
    | Wall
    | Box
    | Stock
    | StockedBox
    | Space
    static member parse source = 
        match source with
        | '@' -> Human 
        | '+' -> HumanOnStock
        | '#' -> Wall
        | '$'-> Box
        | '.' -> Stock
        | '*' -> StockedBox
        | ' ' -> Space
        | _ -> failwithf "%c not expected" source

    static member isHuman = function 
        | Human _ -> true
        | _ -> false

type Position = Position of Element * Coords
    with
        static member parse y x source =
            let element = Element.parse source
            let coords = Coords (X x, y)
            Position (element, coords) 
        static member isHuman (Position (element, _)) = element |> Element.isHuman 
        static member hasCoords c (Position (_,coords)) = c = coords

type Line = Line of Position list
    with 
        static member parse y (source:string) = 
            source 
            |> Seq.mapi (Position.parse (Y y))
            |> Seq.toList
            |> Line
        static member tryHuman(Line elements) = 
            let l = elements |> List.filter Position.isHuman
            if l |> Seq.length > 0 then l |> List.exactlyOne |> Some
            else None
        static member tryCoords c (Line elements) =
            let l = elements |> List.filter (Position.hasCoords c)
            if l |> List.length > 0 then Some (l |> List.exactlyOne)
            else None

type Map = Map of Line list
    with 
        static member parse file = 
            System.IO.File.ReadAllLines(file) 
            |> Seq.mapi Line.parse 
            |> Seq.toList 
            |> Map
        static member human (Map map) = 
            map
            |> List.map Line.tryHuman
            |> List.filter Option.isSome
            |> List.map (fun v -> v.Value)
            |> List.exactlyOne
        static member coords c (Map lines) = 
            lines
            |> List.map (Line.tryCoords c)
            |> List.filter Option.isSome
            |> List.map (fun v -> v.Value)
            |> List.exactlyOne
        static member tryMove m (Position(e, c)) = 
            let (Position(e, c)) = map |> Map.coords newHumanCoords


        static member play move map =
            let human = map |> Map.human
            let (Position(humanE, humanC)) = human

            let newHumanCoords = Move.move move humanC

            let (Position(targetE, targetC)) = map |> Map.coords newHumanCoords

            let newTargetCoords = Move.move move targetC

            let (Position(newTargetE, newTargetC)) = map |> Map.coords newTargetCoords



             
let game = "C:\Users\Clem\Source\Repos\code4fun\Katas\Game.raw"

let map = Map.parse game

let human = map |> Map.human
let (Position(humanE, humanC)) = human

let moveUp = Move.move Move.Up humanC

let target = map |> Map.coords moveUp





//Human X 4 Y 3