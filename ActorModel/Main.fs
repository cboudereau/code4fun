module Main

open ActorModel
open Availpro

let m number hotelId body = 
    let message = { number = number; hotelId = hotelId; body = body }
    { uid = number
      sequenceId = hotelId
      message = message }

let messages = 
    [ m 1 1 "Message1"
      m 2 1 "Message2"
      m 3 2 "Message1"
      m 4 2 "Message2"
      m 5 2 "Message3"
      m 6 2 "Message4"
      m 7 2 "Message5"
      m 8 2 "Message6"
      m 9 1 "Message3" ]

let displayMessage message = printfn "%A" message

let run () = 
    messages 
    |> ActorModel.dispatch 3 displayMessage 
    |> Async.RunSynchronously

[<EntryPoint>]
let main (_) = 
    run ()
    System.Console.ReadLine() |> ignore
    0