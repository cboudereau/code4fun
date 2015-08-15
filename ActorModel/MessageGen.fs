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
            let infinite max = Seq.initInfinite(fun i -> if i < max then i else 0)
            let hotelIds = infinite maxHotels
            let hotelMessageIds = infinite 10000000

            Seq.map3 (fun uniqueId hotelId hotelMessageNumber -> 
                let message = { uid = uniqueId; sequenceId = hotelId; sequenceNumber = hotelMessageNumber }
                { sequenceId = hotelId; uid = uniqueId; message = message }) uniqueIds hotelIds hotelMessageIds
            |> Seq.toList
        
        Arb.generate<NonNegativeInt>
        |> Gen.map(fun (NonNegativeInt max) -> [1 .. max])
        |> Gen.map(gen 10)
    
    static member Values() = RandomMessages.Gen() |> Arb.fromGen