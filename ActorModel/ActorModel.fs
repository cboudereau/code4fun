module ActorModel

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
                let! (reply, message) = inbox.Receive()
                match message with
                | Process receive ->
                    do! receive job
                    reply ()
                    do! listen()
                | Dispose -> 
                    //Dispose Azure session
                    reply ()
                    return ()
            }
        //Create Azure session
        //printfn "%A worker birth" id
        listen()

let actorPool limit factory = 
    let collect actors = 
        let garbage = 
            actors 
            |> Map.toSeq 
            |> Seq.filter(fun (_,(a:Actor<_>)) -> a.CurrentQueueLength = 0) 
            |> Seq.map(
                fun (k, a) -> 
                    async {
                        do! a.PostAndAsyncReply <| fun channel -> channel.Reply, Dispose
                        return k })
            |> Async.Parallel
            |> Async.RunSynchronously
            |> Seq.toList

        actors 
        |> Map.toList 
        |> List.filter(fun (k,_) -> garbage |> List.exists(fun id -> id = k) |> not)
        |> Map.ofList

    Actor.Start <| fun inbox -> 
        let rec listen actors = 
            async {
                let! (reply, key) = inbox.Receive()
                match actors |> Map.tryFind key with
                | Some actor -> 
                    actor |> Some |> reply
                    do! actors |> listen
                | None ->
                    match actors |> Map.toSeq |> Seq.length with
                    | l when l = limit ->
                        let survivors = actors |> collect
                        None |> reply
                        do! survivors |> listen
                    | _ ->
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
                    do! actor.PostAndAsyncReply <| fun channel -> channel.Reply, (h |> receive |> Process) 
                    do! peek t
                | None ->
                    do! peek queue
            | [] -> 
                //Here renew session to get new messages
                //printfn "finished"
                ()
        }
    peek queue