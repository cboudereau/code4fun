module ActorModelProperties
open FsCheck.Xunit
open ActorModel
open MessageGen
open Microsoft.ServiceBus.Messaging

module Async = 
    open System.Threading
    let cancellationTokenSource () = new CancellationTokenSource()
    let cancel (cancellationTokenSource:CancellationTokenSource) = cancellationTokenSource.Cancel()
    let token (cancellationTokenSource:CancellationTokenSource) = cancellationTokenSource.Token

[<Arbitrary(typeof<RandomMessages>)>]
module PropertiesBasedOnRandomMessages = 
    
    type TestMessage = 
        { sessionId:string
          number: string }

    type MessageReceive<'a> = 
        | Get
        | Post of 'a

    let messageReceiver () = 
        Actor.Start <| fun inbox ->
            let rec listen messages =
                async {
                    let! (replyChannel, message) = inbox.Receive()
                    match message with
                    | Get -> 
                        messages |> List.rev |> replyChannel
                        do! listen messages
                    | Post m -> 
                        replyChannel messages
                        do! (m :: messages) |> listen
                }
            listen List.empty
    
    let getMessages (messageReceiver:Actor<_>)  = messageReceiver.PostAndReply <| fun channel -> channel.Reply, Get

    let convertMessage (message:BrokeredMessage) = { sessionId = message.SessionId; number = message |> Azure.getNumber }

    let receiveMessage (messageReceiver:Actor<_>) message = 
        async {
            let messages = messageReceiver.PostAndAsyncReply <| fun channel -> channel.Reply, (message |> convertMessage |> Post)
            return! messages |> Async.Ignore }

    let hasOrderPreserved input output = 
        let messagesPerSession = input |> List.groupBy(fun m -> m.sessionId) |> Map.ofList

        output
        |> List.groupBy(fun m -> m.sessionId)
        |> List.forall(fun (sessionId, l) -> 
            let seqNumbers = messagesPerSession |> Map.find sessionId |> List.map(fun m -> m.number)
            let actual = l |> List.map(fun m -> m.number)
            seqNumbers = actual)

    let waitAllMessages cancellationTokenSource getMessages messages = 
        let messageLength = messages |> List.length
        let rec listenAllMessages () = 
            async{
                let state = getMessages ()
                if state |> List.length = messageLength
                then 
                    cancellationTokenSource |> Async.cancel
                    return state
                else 
                    do! Async.Sleep 100
                    return! listenAllMessages () }
        listenAllMessages () |> Async.RunSynchronously

    let printStats messages = 
        let internalPrintStats messages = 
            let group = messages |> List.groupBy(fun m -> m.sessionId)

            let (sequenceId, count) = 
                group
                |> List.map(
                    fun (id, messages) -> 
                        let messageCount = messages |> List.length
                        id, messageCount)
                |> List.maxBy(fun (_, count) -> count)

            printfn "#%s sessionId with %i messages" sequenceId count
            group |> Seq.length |> printfn "number of sessions %i"

        match messages with
        | [] -> printfn ""
        | _ -> internalPrintStats messages

    let queueName () = System.Guid.NewGuid().ToString()

    let run workerCount messages = 
        let queueName = queueName ()
        
        try
            messages |> Azure.sendAll (Azure.createQueueSender queueName)
            let input = messages |> List.map convertMessage

            input |> printStats

            let outbox = messageReceiver ()
            let receiveMessage = receiveMessage outbox
            let getMessages () = getMessages outbox
            
            let stopWatch = System.Diagnostics.Stopwatch.StartNew()
            
            let cancelToken = Async.cancellationTokenSource()
             
            let dispatcher = queueName |> Azure.createQueueListener |> ActorModel.dispatch workerCount receiveMessage
            Async.Start(dispatcher, cancelToken |> Async.token)

            let output = messages |> waitAllMessages cancelToken getMessages 
            let numberOfMessages = messages |> List.length |> float

            match stopWatch.ElapsedMilliseconds with
            | 0L -> printfn "no messages"
            | elapsed ->  
                printfn "messages per second : %f" (numberOfMessages / (float elapsed) * 1000.)
                printfn ""

            output |> hasOrderPreserved input

        finally
            queueName |> Azure.deleteQueue


    [<Property(Timeout=3000000, MaxTest=1)>]
    let ``All messages respect sequence number order into a sequence`` messages = 
        run 10000 messages

    [<Property(Timeout=3000000, MaxTest=1)>]
    let ``All messages respect sequence number order into a sequence with not enought workers`` messages = 
        run 20 messages
