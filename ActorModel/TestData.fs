module TestData

//Here make a HotelGenerator and a Message generator

//Then produce messages with same hotelid

open FsCheck

open Availpro

open ActorModel

let build (NonNegativeInt hotelId, NonNegativeInt messageId) = 
    let message = { number = messageId; hotelId = hotelId; body = "Hello" }
    { sequenceId = hotelId; id = messageId; message = message }

type RandomMessage = 
    static member Gen() = 
        Arb.generate<NonNegativeInt>
        |> Gen.two
        |> Gen.map build
        |> Gen.listOf

    static member Values() = RandomMessage.Gen() |> Arb.fromGen