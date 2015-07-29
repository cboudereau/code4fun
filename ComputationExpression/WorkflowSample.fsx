type FailureState = 
    | CommunicationFailure of string
    | ParsingFailure of bool
    | ExceptionFailure of exn

type State<'a> = 
    | Success of 'a
    | Failure of FailureState

module AsyncState = 
    let asyncBind f asyncValue = 
        async {
            let! value = asyncValue
            match value with
            | Success a -> return! f a
            | Failure b -> return Failure b }
        

type AsyncStateBuilder()=
    member __.Bind(asyncValue, f) = AsyncState.asyncBind f asyncValue
        
    member __.Return(x) = async { return Success x }
    member __.ReturnFrom(x) = async { return! x }

    member __.Yield(x) = async{ return! x }
    member __.For(x, f) = x |> Seq.map (AsyncState.asyncBind f)

let asyncState = AsyncStateBuilder()

let send requests response = 
    requests
    |> Seq.map(
        function
        | "s" -> async { return response |> Success }
        | v -> async { return Failure (CommunicationFailure v)  })
    
let parse response =
    async {
        printfn "Enter parse with response : %s" response
        match response with
        | "s" -> return Success true
        | _ -> return Failure (ParsingFailure true) }

let asyncResults requests response = 
    asyncState { for s in send requests response do yield parse s  }
    |> Async.Parallel

let displayResults result = 
    match result with
    | Success v -> printfn "Yeah! Success %b" v
    | other -> printfn "Ohhhh No! %A" other

asyncResults ["i";"s"] "s" |> Async.RunSynchronously |> Seq.map displayResults |> Seq.toList