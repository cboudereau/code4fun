type FailureState = 
    | CommunicationFailure of string
    | ParsingFailure of bool

type State<'a> = 
    | Success of 'a
    | Failure of FailureState

type StateBuilder()=
    member __.Bind(asyncValue, f) = 
        async {
                let! value = asyncValue
                match value with
                | Success a -> return! f a
                | Failure b -> return Failure b }
        
    member __.Return(x) = async { return Success x }
    member __.ReturnFrom(x) = async { return! x }

let state = StateBuilder()

let send request response = 
    async {
        match request with
        | "s" -> return Success response
        | v -> return Failure (CommunicationFailure v) }

let parse response =
    async {
        match response with
        | "s" -> return Success true
        | _ -> return Failure (ParsingFailure true) }

let resultAsync = 
    state {
        let! r1 = send "s" "s"
        return! parse r1 }


let result = 
    async {
        let! result = resultAsync
        match result with
        | Success v -> printfn "Yeah! Success %b" v
        | other -> printfn "Ohhhh No! %A" other }

result |> Async.RunSynchronously