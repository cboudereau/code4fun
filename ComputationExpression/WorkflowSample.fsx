type FailureState = 
    | ParsingError of string
    | Failed of bool

type State<'a, 'b> = 
    | Success of 'a
    | Failure of FailureState

type StateBuilder()=
    member __.Bind(x, f) = 
        match x with
        | Success a -> f a
        | Failure b -> Failure b
    member __.Return(x) = Success x

let state = StateBuilder()

let send request = 
    match request with
    | "s" -> Success "s"
    | v -> Failure (ParsingError "s")

let parse response =
    match response with
    | "s" -> Success true
    | v -> Failure (Failed true)

state {
    let! r1 = send "s"
    let! r2 = parse r1
    return r2
}