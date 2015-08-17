module ActorModelProperties
open FsCheck.Xunit
open ActorModel
open MessageGen
open Microsoft.ServiceBus.Messaging
open Microsoft.WindowsAzure

[<Arbitrary(typeof<RandomMessages>)>]
module PropertiesBasedOnRandomMessages = 
    
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
    
    let getMessages (messageReceiver:Actor<_>)  = 
        let asyncGetMessages = messageReceiver.PostAndAsyncReply <| fun channel -> channel.Reply, Get
        asyncGetMessages |> Async.RunSynchronously

    let receiveMessage (messageReceiver:Actor<_>) message = 
        let messages = messageReceiver.PostAndAsyncReply <| fun channel -> channel.Reply, (message |> Post)
        messages |> ignore

    let hasOrderPreserved output messages = 
        let messagesPerSequence = messages |> List.groupBy(Azure.getSessionId) |> Map.ofList

        output
        |> List.groupBy(Azure.getSessionId)
        |> List.forall(fun (sequenceId, l) -> 
            let seqNumbers = messagesPerSequence |> Map.find sequenceId |> List.map(Azure.getSequenceNumber)
            let actual = l |> List.map(Azure.getSequenceNumber)
            seqNumbers = actual)

    let waitAllMessages getMessages messages = 
        let messageLength = messages |> List.length
        let rec listenAllMessages () = 
            async{
                let state = getMessages ()
                if state |> List.length = messageLength
                then return state
                else return! listenAllMessages () }
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

    [<Property(Timeout=3000000)>]
    let ``All messages respect sequence number order into a sequence`` messages = 
        
        messages
        |> Azure.sendAll (Azure.createQueueSender "test-session")

        let outbox = messageReceiver ()
        let receiveMessage = receiveMessage outbox
        let getMessages () = getMessages outbox
        
        let stopWatch = System.Diagnostics.Stopwatch.StartNew()
        
        Azure.createQueueListener "test-session"
        |> ActorModel.dispatch 10000 receiveMessage
        |> Async.RunSynchronously

        let output = messages |> waitAllMessages getMessages 
        let numberOfMessages = messages |> List.length

        match stopWatch.ElapsedMilliseconds with
        | 0L -> printfn "no messages"
        | elapsed ->  
            printfn "messages %i in %ims" numberOfMessages elapsed
            printfn ""

        messages |> hasOrderPreserved output

    [<Property(Timeout=3000000)>]
    let ``All messages respect sequence number order into a sequence with not enought workers`` messages = 
        messages |> printStats 

        let outbox = messageReceiver ()
        let receiveMessage = receiveMessage outbox
        let getMessages () = getMessages outbox
        let numberOfMessages = messages |> List.length

        let stopWatch = System.Diagnostics.Stopwatch.StartNew()
        
        Azure.createQueueListener "test-session" 
        |> ActorModel.dispatch 100 receiveMessage
        |> Async.RunSynchronously

        let output = messages |> waitAllMessages getMessages 
        
        match stopWatch.ElapsedMilliseconds with
        | 0L -> printfn "no messages"
        | elapsed ->  printfn "messages %i in %ims" numberOfMessages elapsed

        messages |> hasOrderPreserved output