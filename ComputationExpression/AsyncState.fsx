module State = 
    type State<'a, 'b> = 
        | Success of 'a
        | Failure of 'b

    let bind (f:'a -> State<'b,'c>) (x:State<'a,'c>) = 
        match x with
        | Success v -> f v
        | Failure f -> Failure f

    let success v = Success v
    let failure f = Failure f

    type StateBuilder() = 
        member __.Bind(x, f) = x |> bind f
        member __.Return(x) = success x
        member __.ReturnFrom(x) = x
    
    let state = StateBuilder()

open State

let hello shouldFailed = 
    if shouldFailed then failwith "Boom"
    else "Hello"

let stateHello shouldFailed = 
    try
        state { return hello shouldFailed }
    with ex -> state { return! failure ex }

let display stateHello shouldFailed = 
    state {
        let! hello = stateHello shouldFailed
        return printfn "%s" hello
    }

match display stateHello false with
| Success _ -> printfn "success"
| Failure ex -> printfn "failed with error %O" ex