module Main

open ActorModel
open Availpro

let m number hotelId body = 
    let message = { number = number; hotelId = hotelId; body = body }
    { id = number
      sequenceId = hotelId
      message = message }

let messages = 
    [ m 1 "H1" "Message1"
      m 2 "H1" "Message2"
      m 3 "H2" "Message1"
      m 4 "H2" "Message2"
      m 5 "H2" "Message3"
      m 6 "H2" "Message4"
      m 7 "H2" "Message5"
      m 8 "H2" "Message6"
      m 9 "H1" "Message3" ]

let displayMessage message = printfn "%A" message

let run () = 
    messages 
    |> ActorModel.consume 3 displayMessage 
    |> Async.RunSynchronously

[<EntryPoint>]
let main (_) = 
    run ()
    System.Console.ReadLine() |> ignore
    0