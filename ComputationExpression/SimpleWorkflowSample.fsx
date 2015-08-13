
type State<'a, 'b> = 
    | Success of 'a
    | Failure

module State = 
    let bind f x =
        match x with
        | Success x -> f x
        | Failure -> Failure

type StateBuilder() =
    member __.Bind(x, f) = State.bind f x
    member __.Return(x) = Success x
    member __.ReturnFrom(x) = x

    member __.Yield(x) = Success x

    member __.For(l, (f: 'a -> State<'b, 'c>)) = l |> Seq.map f

let state = StateBuilder()

let mayBeOne input = 
    state {
        match input with
        | 1 -> return 1
        | other -> return! Failure }

let parse flag value = 
    state {
        let! value' = value
        match flag with
        | true -> return value'
        | false -> return! Failure }

let parses flag value = parse flag value |> Seq.singleton

state { let v = mayBeOne 1 in for p in parses true v do return! p } |> Seq.toList |> printfn "%A"

let test1() = 

    let getContracts() = 
        state {
            return
                [ ("a", "hello") 
                  ("b", "toto") ] |> Map.ofSeq }

    let matchContracts contracts = 
        match contracts |> Map.tryFind "a" with
        | Some value -> state { return value } |> Seq.singleton
        | _ -> state { return! Failure } |> Seq.singleton

    let useContracts contract = state { return contract }
    
    state {
        let! contracts = getContracts()
        let! result = state { return matchContracts contracts }
        return result
    }
test1()

