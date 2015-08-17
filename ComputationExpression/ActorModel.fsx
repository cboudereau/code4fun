module ActorModel =

    open Microsoft.FSharp.Control

    type Actor<'a> = MailboxProcessor<'a>

    type SequenceMessage<'a> = 
        { sequenceId : int
          uid : int
          message : 'a }

    type WorkerMessage<'a> = 
        | Process of 'a
        | Dispose

    let workerFactory job _ = 
        Actor.Start <| fun inbox -> 
            let rec listen() = 
                async {
                    let! message = inbox.Receive()
                    match message with
                    | Process receive ->
                        do! receive job
                        do! listen()
                    | Dispose -> return ()
                }
            //printfn "%A worker birth" id
            listen()

    let actorPool limit factory = 
        let collect actors = 
            let garbage = 
                actors 
                |> Map.toSeq 
                |> Seq.filter(fun (_,(a:Actor<_>)) -> a.CurrentQueueLength = 0) 
                |> Seq.map(fun (k, a) -> a.Post Dispose; k)
                |> Seq.toList

            actors 
            |> Map.toList 
            |> List.filter(fun (k,_) -> garbage |> List.exists(fun id -> id = k) |> not)
            |> Map.ofList

        Actor.Start <| fun inbox -> 
            let rec listen pool = 
                async {
                    let! (reply, key) = inbox.Receive()
                    match pool |> Map.tryFind key with
                    | Some actor -> 
                        actor |> Some |> reply
                        do! pool |> listen
                    | None ->
                        match pool |> Map.toSeq |> Seq.length with
                        | l when l = limit ->
                            let survivors = pool |> collect
                            None |> reply
                            do! survivors |> listen
                        | _ ->
                            let newActor = factory key
                            newActor |> Some |> reply
                            do! pool |> Map.add key newActor |> listen
                }
            listen Map.empty

    let workerPool workerCount job = job |> workerFactory |> actorPool workerCount

    let dispatch workerCount job queue = 
        
        //Here is to have to worker count limit on worker pool
        let sizedWorkerPool = workerPool workerCount job
        let fromWorkerPool id = sizedWorkerPool.PostAndAsyncReply <| fun channel -> (channel.Reply, id)
        
        //Here replace with azure receive, idea is atomitically receive process and commit message asynchronously
        let receive message job = async { return job message }

        let rec peek queue = 
            async {
                match queue with
                | h :: t ->
                    let! maybeActor = h.sequenceId |> fromWorkerPool
                    match maybeActor with
                    | Some actor ->
                        actor.Post (h |> receive |> Process) 
                        do! peek t
                    | None ->
                        do! peek queue
                | [] -> 
                    //Here renew session to get new messages
                    //printfn "finished"
                    ()
            }
        peek queue

module Availpro =

    type Message = { number:int; hotelId:int; body:string }

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

messages 
|> ActorModel.dispatch 3 displayMessage 
|> Async.RunSynchronously
