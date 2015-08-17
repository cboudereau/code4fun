module MessageGen

open ActorModel
open FsCheck
open Microsoft.ServiceBus.Messaging

type Message = 
    { uid : int
      sequenceId : int (*HotelId*)
      sequenceNumber: int (*Number of message in HotelId Sequence*) }

type RandomMessages =
    static member Gen() = 
        let createTestMessage hotelId =
            let message = new BrokeredMessage(sprintf "Message X for hotel %i" hotelId)
            message.SessionId <- string hotelId
            message

        let populateQueue queue = 
            Arb.generate<NonNegativeInt>
            |> Gen.map(fun (NonNegativeInt i) -> i * 5)
            |> Gen.map createTestMessage

        "test-session"
        |> Azure.createQueueSender
        |> populateQueue
    
    static member Values() = RandomMessages.Gen() |> Arb.fromGen