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
        member __.Bind(x, f) = x |> bind f
        member __.Return(x) = success x
        member __.ReturnFrom(x) = x
    
    let state = StateBuilder()

module AsyncStateSeq =  
    
    type AsyncStateSeq<'a, 'b> = seq<Async<State.State<'a, 'b>>>

    let success x : AsyncStateSeq<_, _> = async { return State.success x } |> Seq.singleton
    let failure x : AsyncStateSeq<_, _> = async { return State.failure x } |> Seq.singleton

    type SeqAsyncStateBuilder() = 
        member __.Bind((x:AsyncStateSeq<_, _>), (f:_-> AsyncStateSeq<_, _>)) : AsyncStateSeq<_, _> = failwith "not yet implemented"
        member __.Yield(x) = success x
        member __.YieldFrom(x) = x
        member __.Combine(x1, x2) = x1 |> Seq.append x2

    let state = SeqAsyncStateBuilder()

module Sample = 
    open State

    type Contract = ContractName of string

    type Price = Amount of decimal

    let getMap _ = 
        [ ContractName "rateCode1", Amount 10m
          ContractName "rateCode2", Amount 100m ] |> Map.ofList

    let getContract _ = 
        let map = getMap ()
        map |> State.success

    type Stay = { contract: Contract; amount: Price }

    type Booking = { isSuccess:bool }

    let getStay rateCode = 
        state {
            let! map = getContract ()
            return!
                match map |> Map.tryFind rateCode with
                | Some amount -> { contract = rateCode; amount = amount } |> State.success
                | None -> { isSuccess = false } |> State.failure
        }

module Sample2 = 
    open AsyncStateSeq

    type Contract = ContractName of string

    type Price = Amount of decimal

    let getMap _ = 
        async {
            return 
                [ ContractName "rateCode1", Amount 10m
                  ContractName "rateCode2", Amount 100m ] |> Map.ofList }

    let getContract _: AsyncStateSeq<_, _> = 
        async {
            let! map = getMap ()
            return map |> State.success
        } |> Seq.singleton

    type Stay = { contract: Contract; amount: Price }

    type Booking = { isSuccess:bool }

    let getStay rateCode = 
        state {
            let! map = getContract ()
            yield!
                match map |> Map.tryFind rateCode with
                | Some amount -> { contract = rateCode; amount = amount } |> AsyncStateSeq.success
                | None -> { isSuccess = false } |> AsyncStateSeq.failure
        }