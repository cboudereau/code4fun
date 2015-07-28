type FailureState = 
    | CommunicationFailure of string
    | ParsingFailure of bool

type State<'a> = 
    | Success of 'a
    | Failure of FailureState

type StateBuilder()=
    member __.Bind(x, f) = 
        match x with
        | Success a -> f a
        | Failure b -> Failure b
    member __.Return(x) = Success x

let state = StateBuilder()

let send request response = 
    match request with
    | "s" -> Success response
    | v -> Failure (CommunicationFailure v)

let parse response =
    match response with
    | "s" -> Success true
    | v -> Failure (ParsingFailure true)

let result = 
    state {
        let! r1 = send "s" "i"
        let! r2 = parse r1
        return r2
    }
match result with
| Success v -> printfn "Success %b" v
| other -> printfn "%A" other