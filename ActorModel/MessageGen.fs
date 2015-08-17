module MessageGen

open ActorModel
open FsCheck
open Microsoft.ServiceBus.Messaging

type RandomMessages =
    static member Gen() = 
        let sequence = Seq.initInfinite(fun i -> i).GetEnumerator()
        let next() = 
            sequence.MoveNext() |> ignore
            sequence.Current

        let createTestMessage (NonNegativeInt hotelId) =
            let message = new BrokeredMessage(sprintf "Message X for hotel %i" hotelId)
            message.SessionId <- string hotelId
            message.Properties.[Azure.number] <- next () |> string

            message

        Arb.generate<NonNegativeInt>
        |> Gen.map createTestMessage
        |> Gen.listOfLength 10


    static member Values() = RandomMessages.Gen() |> Arb.fromGen