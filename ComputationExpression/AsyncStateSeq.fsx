module State = 
    type State<'a, 'b> = 
        | Success of 'a
        | Failure of 'b

    let bind f x = 
        match x with
        | Success v -> f v
        | Failure f -> Failure f

    let success v = Success v
    let failure f = Failure f

    type StateBuilder() =
        member __.Bind(x, f) = bind f x
        member __.Return(x) = success x
        member __.ReturnFrom(x) = x

    let state = StateBuilder()

module AsyncState = 
    open State
    
    let success v = async { return State.success v }
    let failure f = async { return State.failure f }

    let bind f x = 
        async { 
            let! x' = x
            match x' with
            | Success v -> return! f v
            | Failure f -> return! failure f
        }

    type AsyncStateBuilder() = 
        member __.Bind(x, f) = bind f x
        member __.Return(x) = success x
        member __.ReturnFrom(x) = x

    let asyncState = AsyncStateBuilder()

module AsyncStateSample = 
    open AsyncState

    let getSourceMessage isFailure = asyncState { if isFailure then return! failure "Fuck" else return "Hello" }

    let print message = asyncState { let! m = message in return printfn "%A" m }

    getSourceMessage false |> print |> Async.RunSynchronously

module StateSample = 
    open State
    let getSourceMessage isFailing = state { if isFailing then return! failure "failed" else return "Hello" }

    let print message = state{ let! m = message in return printfn "%A" m }

    getSourceMessage false |> print

