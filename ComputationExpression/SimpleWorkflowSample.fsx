
type State<'a, 'b> = 
    | Success of 'a
    | Failure of 'b

module State = 
    let bind f x =
        match x with
        | Success x -> f x
        | Failure f -> Failure f

type StateBuilder() =
    member __.Bind(x, f) = State.bind f x
    member __.Return(x) = Success x
    member __.ReturnFrom(x) = x

    member __.For(l, (f: 'a -> State<'b, 'c>)) = l |> Seq.map f

let state = StateBuilder()

let mayBeOne input = 
    state {
        match input with
        | 1 -> return 1
        | other -> return! Failure other }

let parse flag value = 
    state {
        let! value' = value
        match flag with
        | true -> return value'
        | false -> return! Failure value' }

let parses flag value = parse flag value |> Seq.singleton

state { let v = mayBeOne 1 in for p in parses true v do return! p } |> Seq.toList |> printfn "%A"