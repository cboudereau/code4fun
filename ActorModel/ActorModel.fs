module ActorModel

open System
open Microsoft.FSharp.Control

type Agent<'a> = MailboxProcessor<'a>

type SequenceMessage<'a> = 
    { sequenceId : int
      uid : int
      message : 'a }

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

let actorPool factory limit = 
    Agent.Start <| fun inbox -> 
        let rec listen actors = 
            async {
                let suicideSignal id = inbox.PostAndAsyncReply <| fun channel -> (channel.Reply, Leave id)
                let! (replyChannel, stateFullMessage) = inbox.Receive()
                match stateFullMessage with
                | Take key -> 
                    match actors |> Map.tryFind key with
                    | Some actor -> 
                        actor |> Some |> replyChannel
                        do! actors |> listen
                    | None ->
                        if actors |> Map.toSeq |> Seq.length > limit 
                        then 
                            do! Async.Sleep 100
                            None |> replyChannel 
                            do! actors |> listen
                        else
                            let newActor = factory suicideSignal key
                            newActor |> Some |> replyChannel
                            do! actors |> Map.add key newActor |> listen
                | Leave key -> 
                    let candidate = actors |> Map.find key
                    candidate |> Some |> replyChannel 
                    printfn "%A Left" key
                    do! actors |> Map.remove key |> listen
            }
        listen Map.empty

let workerPool workerCount job = actorPool (workerFactory job) workerCount

let consume workerCount job queue = 
    
    //Here is to have to worker count limit on worker pool
    let limitedWorkerPool = workerPool workerCount job
    let fromWorkerPool id = limitedWorkerPool.PostAndAsyncReply <| fun channel -> (channel.Reply, id)
    
    //Here replace with azure receive, idea is atomitically receive process and commit message asynchronously
    let receive message job = async { return job message }

    let rec peek queue = 
        async {
            match queue with
            | h :: t ->
                let! maybeActor = Take h.sequenceId |> fromWorkerPool
                match maybeActor with
                | Some actor -> 
                    actor.Post (receive h)
                    do! peek t
                | None -> do! peek queue
            | [] -> 
                //Here renew session to get new messages
                printfn "finished"
        }
    peek queue