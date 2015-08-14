﻿open System
open Microsoft.FSharp.Control

type Agent<'a> = MailboxProcessor<'a>

type Message = { number:int; hotelId:string; body:string }

let nm number hotelId body = { number = number; hotelId = hotelId; body = body }

let messages = 
    [ nm 1 "H1" "Message1"
      nm 2 "H1" "Message2"
      nm 3 "H2" "Message1"
      nm 4 "H2" "Message2"
      nm 5 "H2" "Message3"
      nm 6 "H2" "Message4"
      nm 7 "H2" "Message5"
      nm 8 "H2" "Message6"
      nm 9 "H1" "Message3" ]

let rand = Random()
let next () = rand.Next()

type StateFullActorMessage<'a> = 
    | Take of 'a
    | Leave of 'a

let workerFactory job suicideSignal id = 
    Agent.Start <| fun inbox -> 
        let rec listen() = 
            async {
                try
                    let! receive = inbox.Receive(1000)
                    do! receive job
                    do! listen()
                with
                | :? TimeoutException as ex -> 
                    printfn "%A should Dead" id
                    let! candidate = suicideSignal id
                    candidate |> ignore
            }
        printfn "%A worker birth" id
        listen()

let displayMessage message = printfn "%A" message

let actorPool factory = 
    Agent.Start <| fun inbox -> 
        let rec listen actors = 
            async {
                let suicideSignal id = inbox.PostAndAsyncReply <| fun channel -> (channel.Reply, Leave id)
                let! (replyChannel, stateFullMessage) = inbox.Receive()
                match stateFullMessage with
                | Take key -> 
                    match actors |> Map.tryFind key with
                    | Some actor -> 
                        replyChannel actor
                        do! actors |> listen 
                    | None ->
                        let newActor = factory suicideSignal key
                        replyChannel newActor
                        do! actors |> Map.add key newActor |> listen
                | Leave key -> 
                    let candidate = actors |> Map.find key
                    replyChannel candidate
                    printfn "%A Left" key
                    do! actors |> Map.remove key |> listen
            }
        listen Map.empty

let workerPool = actorPool (workerFactory displayMessage)

let fromWorkerPool id = workerPool.PostAndAsyncReply <| fun channel -> (channel.Reply, id)

let receive message job = async { return job message }

let rec peek queue = 
    async {
        match queue with
        | h :: t ->
            let! actor = Take h.hotelId |>fromWorkerPool
            actor.Post (receive h)
            do! peek t
        | [] -> printfn "finished"
    }
        
peek messages |> Async.RunSynchronously