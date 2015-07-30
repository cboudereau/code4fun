type State<'a> = 
    | Success of 'a
    | Failure

let bind (f:'a -> Async<State<'b>>) (x:Async<State<'a>>) =
    async {
        let! v = x
        match v with
        | Success v' -> return! f v'
        | Failure -> return Failure
    }

type StateBuilder() = 
    member __.Bind(x,f) = bind f x
    member __.Return(x) = async { return Success x }
    member __.ReturnFrom(x) = x
    member __.Yield(x) = async { return Success x }
    member __.YieldFrom(x) = x
    member __.For(x, f) = x |> Seq.map (bind f)

let state = StateBuilder()

let getRequest flag r =
    state {
        match flag with
        | true -> return r
        | _ -> return! async { return Failure }
    }

let getRequests flag request = 
    state {
        for r in [request] do
            yield!
                match flag with
                | true -> state { return r }
                | _ -> state { return! async { return Failure } }
    }

let getMRequests flag requests = 
    requests
    |> Seq.collect(fun v -> getRequests flag v)