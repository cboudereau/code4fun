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

module AsyncState = 
    open State
    type AsyncState<'a, 'b> = Async<State<'a, 'b>>

    let success x : AsyncState<_, _> = async { return State.success x }
    let failure x : AsyncState<_, _> = async { return State.failure x }

    let bind f x = 
        async {
            let! x' = x
            match x' with
            | Success v -> return! f v
            | Failure f -> return Failure f 
        }

    type AsyncStateBuilder() = 
        member __.Bind(x, f) = x |> bind f
        member __.Return(x) = success x
        member __.ReturnFrom(x) = x
    
    let asyncState = StateBuilder()

module AsyncStateSeq = 
    open State
    open AsyncState
    type AsyncStateSeq<'a, 'b> = AsyncState<'a, 'b> seq

    let loop (f:'a->AsyncState<'b, 'c>) (x:'a seq) = 
        x 
        |> Seq.map f
        |> ignore

    type AsyncPartialSeqBuilder() = 
        member __.For(x, f) = loop f x
        member __.Combine(x1, x2)  = Seq.append x1 x2
        member __.Delay(f) = f()
    
    let asyncPartialSeq = AsyncPartialSeqBuilder()

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

module Monads = 
    type State<'a, 'b> = 
        | Success of 'a
        | Failure of 'b
    
    let bindChoice f x = 
        match x with
        | Choice1Of3 v | Choice3Of3 v -> f v
        | Choice2Of3 v -> Choice2Of3 v

    let bind f x = 
        match x with
        | Success v -> f v
        | Failure f -> Failure f

    let success v = Success v
    let failure f = Failure f

    type StateList<'a, 'b> = List of State<'a, 'b> seq

    let isFailure = function
        | Success _ -> false
        | Failure _ -> true

    let partialBind f (List x) = x |> Seq.map (bind f) |> List
    let successBind f (List x) = 
        match x |> Seq.filter isFailure |> Seq.length > 0 with
        | false -> x |> Seq.map (bind f) |> List
        | true -> x |> List

    let combine x1 x2 = Seq.append x1 x2 |> List

    let successList x = success x |> Seq.singleton |> List

    type StateBuilder() = 
        member __.Bind(x, f) = bind f x
        member __.Return(x) = success x
        member __.ReturnFrom(x) = x

    let state = StateBuilder()

    type FullStatesBuilder() = 
        member __.For(x, f) = successBind f x
        member __.Combine(List x1, List x2) = combine x1 x2
        member __.Yield(x) = successList x
        member __.YieldFrom(x) = x
        member __.Delay(f) = f()

    let fullStates = FullStatesBuilder()

    type PartialStatesBuilder() =
        member __.For(x, f) = partialBind f x
        member __.Combine(x1, x2) = combine x1 x2
        member __.Yield(x) = successList x
        member __.YieldFrom(x) = x
        member __.Delay(f) = f()

    let partialStates = PartialStatesBuilder()

    type AllPartialStatesBuilder() = 
        member __.Bind(x, f) =  bindChoice f x
        member __.Return(x) = 
        member __.ReturnFrom(x) = x

        member __.For(x, f) = successBind f x
        member __.Combine(List x1, List x2) = combine x1 x2
        member __.Yield(x) = successList x
        member __.YieldFrom(x) = x
        member __.Delay(f) = f()

    let sample = AllPartialStatesBuilder() 

