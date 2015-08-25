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
    type AsyncState<'a, 'b> = Async<State.State<'a, 'b>>

    let success x : AsyncState<_, _> = async { return State.success x }
    let failure x : AsyncState<_, _> = async { return State.failure x }

module AsyncStateSeq =  
    
    open AsyncState
        
    type AsyncStateSeq<'a, 'b> = AsyncState<'a, 'b> seq

    let success x = AsyncState.success x |> Seq.singleton
    let failure x = AsyncState.failure x |> Seq.singleton

    let empty = Seq.empty

    type AsyncStateSeqBuilder() = 
        member __.Bind((x:AsyncStateSeq<_, _>), (f:_-> AsyncStateSeq<_, _>)) : AsyncStateSeq<_, _> = failwith "not yet implemented" 
        member __.For(x: seq<_>, (f:_-> AsyncStateSeq<_, _>)) : AsyncStateSeq<_, _> = failwith "not yet implemented"
        
        member __.Combine(x1,x2) = x2 |> Seq.append x1

        member __.Yield(x) = success x
        member __.YieldFrom(x) = x
        member __.Delay(f) = f()

    let state = AsyncStateSeqBuilder()

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

    type Booking = { isSuccess:bool; stays : Stay seq }

    let getStay rateCode = 
        state {
            let! map = getContract ()
            yield!
                match map |> Map.tryFind rateCode with
                | Some amount -> { contract = rateCode; amount = amount } |> AsyncStateSeq.success
                | None -> { isSuccess = false; stays = Seq.empty } |> AsyncStateSeq.failure
        }

    type BmsBooking = { establishments: string list }

    let bookings _ = state { yield { establishments = [ "rateCode1"; "rateCode2" ] } }

    let getBookings _ = 
        state {
            let! booking = bookings ()
            for b in  booking.establishments do
                let! stays = b |> ContractName |> getStay
                yield { isSuccess = true; stays = stays }
        }

    let getBookingsRec bookings =    
        let rec getBookingRec bookings = 
            state {
                match bookings with
                | head :: tail -> 
                    let! stays = head |> ContractName |> getStay
                    yield { isSuccess = true; stays = stays }
                    yield! tail |> getBookingRec
                | [] -> yield! AsyncStateSeq.empty
            } 
        bookings |> Seq.toList |> getBookingRec

    let getBookings2 _ =
        state {
            let! booking = bookings ()
            let bs = booking.establishments
            yield! bs |> getBookingsRec
        }