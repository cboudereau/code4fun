module ActorModel

open System
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
                | Dispose -> ()
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
        |> Map.toSeq 
        |> Seq.filter(fun (k,_) -> garbage |> Seq.exists(fun id -> id = k))
        |> Map.ofSeq

    Actor.Start <| fun inbox -> 
        let rec listen actors = 
            async {
                let! (reply, key) = inbox.Receive()
                match actors |> Map.tryFind key with
                | Some actor -> 
                    actor |> Some |> reply
                    do! actors |> listen
                | None ->
                    if actors |> Map.toSeq |> Seq.length > limit 
                    then 
                        let remain = actors |> collect
                        do! Async.Sleep 10
                        None |> reply
                        do! remain |> listen
                    else
                        let newActor = factory key
                        newActor |> Some |> reply
                        do! actors |> Map.add key newActor |> listen
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