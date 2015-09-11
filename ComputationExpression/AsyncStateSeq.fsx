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
        member __.For(x, f) = x |> Seq.map f
        member __.Yield(x) = x |> success
        member __.YieldFrom(x) = x
        member __.Combine(x1, x2) = 
            seq { yield x1; yield success x2 }
        member __.ReturnFrom(x) = x
        member __.Delay(f) = f()
        member __.Zero() = Seq.empty

    let asyncState = AsyncStateBuilder()

module AsyncStateSample = 
    open AsyncState

    let getSourceMessage isFailure = asyncState { if isFailure then return! failure "Fuck" else return "Hello" }

    let print message = asyncState { let! m = message in return printfn "%A" m }

    getSourceMessage true |> print |> Async.RunSynchronously

    let getStay _ = asyncState { return "stay" |> Seq.singleton }
    let getRoom _ = 
        let rates = ["1";"2"] |> Seq.map success
        asyncState { 
            for v in rates do    
                yield v
        }

    let getBookings bookings = 
        
        let rec getBookings bookings = 
            asyncState {
                match bookings with
                | h :: tail -> 
                    yield h
                    yield! getBookings tail
                | [] -> ()
            }
        getBookings bookings

    let parse _ =
        asyncState{
            for b in getBookings ["1";"2"] do
                for r in b |> getRoom do
                    yield! r
        }
        

module StateSample = 
    open State
    let getSourceMessage isFailing = state { if isFailing then return! failure "failed" else return "Hello" }

    let print message = state{ let! m = message in return printfn "%A" m }

    getSourceMessage false |> print


