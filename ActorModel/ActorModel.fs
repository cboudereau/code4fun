module ActorModel

open Microsoft.FSharp.Control
open Microsoft.ServiceBus.Messaging;
open Microsoft.WindowsAzure;

type Actor<'a> = MailboxProcessor<'a>

type WorkerMessage = 
    | Process 
    | Dispose

let workerFactory (azureClient:QueueClient) sessionId job = 
    Actor.Start <| fun inbox -> 
        let rec listen (session:MessageSession) = 
            async {
                let! (reply, message) = inbox.Receive()
                
                match message with
                | Process ->
                    reply ()
                    //TODO handle session expiration
                    let message = session.Receive()
                    job message
                    //TODO handle errors
                    message.Complete()
                    do! listen session
                | Dispose -> 
                    //TODO handle session expiration
                    session.Close()
                    reply ()
                    return ()
            }
        let session = azureClient.AcceptMessageSession(sessionId:string)
        listen session

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
                        let newActor = factory ()
                        newActor |> Some |> reply
                        do! actors |> Map.add key newActor |> listen
            }
        listen Map.empty

let workerPool azureClient workerCount job sessionId  = 
    let wf () = workerFactory azureClient sessionId job 
    actorPool workerCount wf

let dispatch workerCount job queue = 
    //Here is to have to worker count limit on worker pool
    let sizedWorkerPool = workerPool queue workerCount job
    let fromWorkerPool sessionId = (sizedWorkerPool sessionId).PostAndAsyncReply <| fun channel -> (channel.Reply, sessionId)
    
    //Here replace with azure receive, idea is atomitically receive process and commit message asynchronously
    let receive message job = async { return job message }

    let rec azurePeek queue = 
        let rec peek (queue : QueueClient) = 
            async{
                match queue.Peek() with
                | null -> 
                    do! Async.Sleep 100
                    return! peek queue
                | message -> return message 
            }
        

        async {
            let! message = peek queue
            
            let! maybeActor = message.SessionId |> fromWorkerPool

            match maybeActor with
            | Some actor -> 
                do! actor.PostAndAsyncReply <| fun channel -> channel.Reply, Process
            | None -> 
                do! Async.Sleep 10
                do! azurePeek queue
        }

//        async {
//            let! message = peek queue
//
//            let! maybeActor = message.SessionId |> fromWorkerPool
//                match maybeActor with
//                | Some actor ->
//                    do! actor.PostAndAsyncReply <| fun channel -> channel.Reply, (h |> receive |> Process) 
//                    do! peek t
//                | None ->
//                    do! peek queue
//        }
    azurePeek queue