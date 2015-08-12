
type State<'a, 'b> = 
    | Success of 'a
    | Failure of 'b

type StateBuilder() =
    member __.Bind(x, f) = 
        match x with
        | Success v -> f v
        | Failure f -> f
    member __.Return(x) = Success x
    member __.ReturnFrom(x) = x

let state = StateBuilder()

let mayBeOne input = 
    state {
        match input with
        | 1 -> return 1
        | other -> return! Failure "bad" }

let parse flag value = 
    state {
        match flag with
        | true -> return value
        | false -> return! Failure "bad" }

let test = 
    state {
        let v = mayBeOne 1
        let p = parse true 1
        return! p
    }
    