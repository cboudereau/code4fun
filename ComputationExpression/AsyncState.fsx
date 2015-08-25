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

    type AsyncStateSeq<'a, 'b> = AsyncState<'a, 'b> seq

    let success x : AsyncState<_, _> = async { return State.success x }
    let failure x : AsyncState<_, _> = async { return State.failure x }

    let bind f (x:AsyncState<'d, 'e>) : AsyncState<'f, 'g> = failwith "not yet implemented"
        
    let loop (f:_-> AsyncState<_, _>) (x: seq<_>) : AsyncState<_, _> = failwith "not yet implemented"

    let combine x1 x2 = x1 |> Seq.append x2 

    type SeqAsyncStateBuilder() = 
        member __.Bind((x: AsyncState<_, _>), (f:_-> AsyncState<_, _>)) : AsyncState<_, _> = bind f x

        member __.Return(x) = x |> success
        member __.ReturnFrom(x) = x

        member __.For(x: seq<_>, (f:_-> AsyncState<_, _>)) : AsyncState<_, _> = failwith "not yet implemented"
        member __.Yield(x) = x |> success |> Seq.singleton
        member __.YieldFrom(x) = x
        member __.Combine(x1, x2) = combine x1 x2

        member __.Delay(f) = f()

    let state = SeqAsyncStateBuilder()

module Sample2 = 
    open AsyncState

    type Contract = ContractName of string

    type Price = Amount of decimal

    let getMap _ = 
        async {
            return 
                [ ContractName "rateCode1", Amount 10m
                  ContractName "rateCode2", Amount 100m ] |> Map.ofList }

    let getContract _: AsyncState<_, _> = 
        async {
            let! map = getMap ()
            return map |> State.success
        }

    type Stay = { contract: Contract; amount: Price }

    type Booking = { isSuccess:bool; stays: Stay seq }

    let getStay rateCode = 
        state {
            let! map = getContract ()
            yield!
                match map |> Map.tryFind rateCode with
                | Some amount -> { contract = rateCode; amount = amount } |> Seq.singleton |> AsyncState.success 
                | None -> { isSuccess = false; stays = Seq.empty } |> Seq.singleton |> AsyncState.failure 
        }

    type BmsBooking = { establishments: string list }

    let bookings _ = state { return { establishments = [ "rateCode1"; "rateCode2" ] } }

    let getBookings _ = 
        state {
            let! booking = bookings ()
            for b in  booking.establishments do
                yield! b |> ContractName |> getStay
        }
    
    let getBookings2 _ = 
        state {
            let! booking = bookings ()
            for b in  booking.establishments do
                let! stays = b |> ContractName |> getStay
                for s in stays do
                    yield s
        }

    let getBookings3 _ = 
        
        let getBookingsRec bookings = 
            let rec getBookings bookings = 
                state {
                    match bookings with
                    | head :: tail ->
                        let! stays = head |> ContractName |> getStay
                        yield { isSuccess = true; stays = stays }
                        yield! getBookings tail
                    | [] -> yield! Seq.empty
                }
            bookings |> Seq.toList |> getBookings

        state{
            let! b = bookings ()

            yield! b.establishments |> getBookingsRec
        }