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
        static member move m coords = 
            let f = 
                match m with
                | Up -> Coords.up
                | Down -> Coords.down
                | Left -> Coords.left
                | Right -> Coords.right
            f coords

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

type Game = Game of Map<Coords, Element>
    with 
        static member parsePosition y x source =
            let element = Element.parse source
            let coords = Coords (X x, y)
            (coords, element) 

        static member parseLine y (source:string) = 
            source 
            |> Seq.mapi (Game.parsePosition (Y y))
            |> Seq.toList

        static member element coords (Game game) = game |> Map.tryFind coords

        static member human (Game game) = 
            game 
            |> Map.toList
            |> List.filter(fun (_, e) -> match e with Human -> true | _ -> false)
            |> List.exactlyOne

        static member impact swap move human (Game game) = 
            
            let tryMove (c, e) m (Game game) = 
                let leave = function
                    | HumanOnStock | StockedBox -> Stock
                    | remain -> remain

                let go from tO = 
                    match from, tO with
                    | Human, Stock -> HumanOnStock
                    | Box, Stock -> StockedBox
                    | remain, _ -> remain

                let target = Move.move m c
                match e, game |> Map.find target with
                | Box, Box | _, Wall | _, Human -> None
                | _, eT -> 
                    game
                    |> Map.remove target
                    |> Map.add target (go e eT)
                    |> Map.remove c
                    |> Map.add c (leave eT)
                    |> Game
                    |> Some


            let humanCoords = human |> fst
            let targetCoords = Move.move move humanCoords
            match game |> Map.find targetCoords with
            | Space -> 
                tryMove
                swap (targetCoords, Space) human (Game game)
            | Box -> 
                match game |> Game |> Game.impact swap move (targetCoords, Box) with
                | Some game -> swap ()
                | None -> Game game
                
            | Wall -> Game game
                

        static member game action game = 
            let tryMove action = 
                let human = game |> Game.human
                let tryMove action = 
                    let humanCoords = human |> fst
                    let targetCoords = Move.move action humanCoords
                    let targetElement = Game.element targetCoords
                    let target = game |> element humanCoords


        static member parse file = 
            let it i = i
            System.IO.File.ReadAllLines(file) 
            |> Seq.mapi Game.parseLine 
            |> Seq.toList 
            |> List.collect it
            |> Map.ofList
            |> Game