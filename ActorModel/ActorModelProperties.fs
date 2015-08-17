module ActorModelProperties
open FsCheck.Xunit
open ActorModel
open MessageGen
open Microsoft.ServiceBus.Messaging
open Microsoft.WindowsAzure

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
        let messages = messageReceiver.PostAndReply <| fun channel -> channel.Reply, (message |> convertMessage |> Post)
        messages |> ignore

    let hasOrderPreserved output messages = 
        let messagesPerSequence = messages |> List.groupBy(fun m -> m.sessionId) |> Map.ofList

        output
        |> List.groupBy(fun m -> m.sessionId)
        |> List.forall(fun (sequenceId, l) -> 
            let seqNumbers = messagesPerSequence |> Map.find sequenceId |> List.map(fun m -> m.number)
            let actual = l |> List.map(fun m -> m.number)
            seqNumbers = actual)

    let waitAllMessages getMessages messages = 
        let messageLength = messages |> List.length
        let rec listenAllMessages () = 
            async{
                let state = getMessages ()
                if state |> List.length = messageLength
                then return state
                else 
                    do! Async.Sleep 100
                    return! listenAllMessages () }
        listenAllMessages () |> Async.RunSynchronously

    let printStats messages = 
        let internalPrintStats messages = 
            let group = messages |> List.groupBy(Azure.getSessionId)

            let (sequenceId, count) = 
                group
                |> List.map(
                    fun (id, messages) -> 
                        let messageCount = messages |> List.length
                        id, messageCount)
                |> List.maxBy(fun (_, count) -> count)
            printfn "sequenceId : %s with %i messages" sequenceId count
            group |> Seq.length |> printfn "printfn number of Sequences %i"

        match messages with
        | [] -> printfn ""
        | _ -> internalPrintStats messages

    let queueName () = System.Guid.NewGuid().ToString()

    let run workerCount messages = 
        let queueName = queueName ()
        
        try
            messages |> Azure.sendAll (Azure.createQueueSender queueName)

            let outbox = messageReceiver ()
            let receiveMessage = receiveMessage outbox
            let getMessages () = getMessages outbox
            
            let stopWatch = System.Diagnostics.Stopwatch.StartNew()
            
            Azure.createQueueListener queueName
            |> ActorModel.dispatch workerCount receiveMessage
            |> Async.Start

            let output = messages |> waitAllMessages getMessages 
            let numberOfMessages = messages |> List.length

            match stopWatch.ElapsedMilliseconds with
            | 0L -> printfn "no messages"
            | elapsed ->  
                printfn "messages %i in %ims" numberOfMessages elapsed
                printfn ""

            let intput = messages |> List.map convertMessage

            intput |> hasOrderPreserved output

        finally
            queueName |> Azure.deleteQueue


    [<Property(Timeout=3000000, MaxTest=1)>]
    let ``All messages respect sequence number order into a sequence`` messages = 
        run 10000 messages

    [<Property(Timeout=3000000, MaxTest=1)>]
    let ``All messages respect sequence number order into a sequence with not enought workers`` messages = 
        run 5 messages
