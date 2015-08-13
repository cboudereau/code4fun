type State<'a> = 
    | Success of 'a
    | Failure

module State = 
    let bind f x = 
        x 
        |> Seq.map(fun x' -> 
            async {
                let! x'' = x'
                return
                    match x'' with
                    | Success v -> f v
                    | Failure -> Failure            
            })

    let result x = async { return Success x }

type StateBuilder () =
    member __.Bind (x, f) = State.bind f x
    member __.Yield (x) = State.result x
    member __.YieldFrom(x) = x

let state = StateBuilder()
