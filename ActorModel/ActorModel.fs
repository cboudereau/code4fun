module ActorModel

open Microsoft.FSharp.Control
open Microsoft.ServiceBus.Messaging;
open Microsoft.WindowsAzure;

type Actor<'a> = MailboxProcessor<'a>

type WorkerMessage = 
    | Process 
    | Dispose

let workerFactory (azureClient:QueueClient) job sessionId = 
    Actor.Start <| fun inbox -> 
        let rec listen (session:MessageSession) = 
            async {
                try
                    let! (reply, message) = inbox.Receive()
                    
                    match message with
                    | Process ->
                        reply ()
                        //TODO handle session expiration
                        let! message = session.ReceiveAsync() |> Async.AwaitTask
                        do! job message
                        //TODO handle errors
                        do! message.CompleteAsync() |> Async.AwaitTask
                        do! listen session
                    | Dispose -> 
                        //TODO handle session expiration
                        session.Close()
                        reply ()
                        return ()
                with ex -> printfn "Exception occured in worker --> %A" ex
            }
        let session = azureClient.AcceptMessageSession(sessionId:string)
        listen session

let actorPool limit factory = 
    let collect actors = 
        async {
            let! garbage = 
                actors 
                |> Map.toSeq 
                |> Seq.filter(fun (_,(a:Actor<_>)) -> a.CurrentQueueLength = 0) 
                |> Seq.map(
                    fun (k, a) -> 
                        async {
                            do! a.PostAndAsyncReply <| fun channel -> channel.Reply, Dispose
                            return k })
                |> Async.Parallel

            return
                actors 
                |> Map.toList 
                |> List.filter(fun (k,_) -> garbage |> Array.exists(fun id -> id = k) |> not)
                |> Map.ofList }

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
                        let! survivors = pool |> collect
                        None |> reply
                        do! survivors |> listen
                    | _ ->
                        let newActor = factory key
                        newActor |> Some |> reply
                        do! pool |> Map.add key newActor |> listen
            }
        listen Map.empty

let workerPool azureClient workerCount job = 
    job |> workerFactory azureClient |> actorPool workerCount

let dispatch workerCount job queue = 
    //Here is to have to worker count limit on worker pool
    let sizedWorkerPool = workerPool queue workerCount job
    let fromWorkerPool sessionId = sizedWorkerPool.PostAndAsyncReply <| fun channel -> (channel.Reply, sessionId)
    
    let rec peek queue = 
        let rec peekMessage (queue : QueueClient) = 
            async{
                match queue.Peek() with
                | null -> 
                    do! Async.Sleep 50
                    return! peekMessage queue
                | message -> return message 
            }

        let rec getActor sessionId = 
            async {
                let! maybeActor = sessionId |> fromWorkerPool
                match maybeActor with
                | Some actor -> return actor
                | None -> 
                    do! Async.Sleep 100
                    return! getActor sessionId
            }

        async {
            let! message = peekMessage queue
            let! actor = message.SessionId |> getActor
            do! actor.PostAndAsyncReply <| fun channel -> channel.Reply, Process
            do! peek queue
        }

    peek queue