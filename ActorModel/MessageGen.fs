module MessageGen

open ActorModel
open FsCheck

type Message = 
    { uid : int
      sequenceId : int (*HotelId*)
      sequenceNumber: int (*Number of message in HotelId Sequence*) }

type RandomMessages =
    static member Gen() = 
        let gen maxHotels uniqueIds = 
            let infinite max = Seq.initInfinite(fun i -> if i > max then 0 else i)
            let hotelIds = infinite maxHotels
            let hotelMessageIds = Seq.initInfinite(fun i -> i)

            Seq.map3 (fun uniqueId hotelId hotelMessageNumber -> 
                let message = { uid = uniqueId; sequenceId = hotelId; sequenceNumber = hotelMessageNumber }
                { sequenceId = hotelId; uid = uniqueId; message = message }) uniqueIds hotelIds hotelMessageIds
            |> Seq.toList
        
        Arb.generate<NonNegativeInt>
        |> Gen.map(fun (NonNegativeInt value) -> [1 .. (value * 500)])
        |> Gen.map(gen 500)
    
    static member Values() = RandomMessages.Gen() |> Arb.fromGen