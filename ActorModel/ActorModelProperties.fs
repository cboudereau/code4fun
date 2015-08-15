module ActorModelProperties
open FsCheck.Xunit
open ActorModel
open MessageGen
    
[<Arbitrary(typeof<RandomMessages>)>]
module PropertiesBasedOnRandomMessages = 
    
    type MessageReceive<'a> = 
        | Get
        | Put of 'a

    let messageReceiver () = 
        Actor.Start <| fun inbox ->
            let rec listen messages =
                async {
                    let! (replyChannel, message) = inbox.Receive()
                    match message with
                    | Get -> 
                        replyChannel messages 
                        do! listen messages
                    | Put m -> 
                        replyChannel messages
                        do! (m :: messages) |> listen
                }
            listen List.empty

    let getMessages (messageReceiver:Actor<_>)  = 
        let asyncGetMessages = messageReceiver.PostAndAsyncReply <| fun channel -> channel.Reply, Get
        asyncGetMessages |> Async.RunSynchronously

    let receiveMessage (messageReceiver:Actor<_>) message = 
        let messages = messageReceiver.PostAndAsyncReply <| fun channel -> channel.Reply, (Put message)
        messages |> Async.Ignore |> Async.RunSynchronously

    let hasOrderPreserved output messages = 
        let messagesPerSequence = messages |> List.groupBy(fun m -> m.message.sequenceId) |> Map.ofList

        output
        |> List.groupBy(fun m -> m.message.sequenceId)
        |> List.forall(fun (sequenceId, l) -> 
            let seqNumbers = messagesPerSequence |> Map.find sequenceId |> List.map(fun m -> m.message.sequenceNumber)
            let actual = l |> List.map(fun m -> m.message.sequenceNumber)
            let isTrue = seqNumbers = actual
            isTrue)

    let waitAllMessages getMessages messages = 
        let rec listenAllMessages () = 
            async{
                let state = getMessages ()
                if state |> List.length = (messages |> List.length)
                then return (state  |> List.rev)
                else 
                    do! Async.Sleep 2
                    return! listenAllMessages () }
        listenAllMessages () |> Async.RunSynchronously


    [<Property(Timeout=3000000)>]
    let ``All messages respect sequence number order into a sequence`` (messages:SequenceMessage<Message> list) = 
        let outbox = messageReceiver ()
        let receiveMessage = receiveMessage outbox
        let getMessages () = getMessages outbox
        let numberOfMessages = messages |> List.length |> int64

        let stopWatch = System.Diagnostics.Stopwatch.StartNew()
        messages 
        |> ActorModel.dispatch 10000 receiveMessage
        |> Async.RunSynchronously

        let output = messages |> waitAllMessages getMessages 
        
        match stopWatch.ElapsedMilliseconds with
        | 0L -> printfn "no messages"
        | _ -> 
            printfn "messages %i in %ims" numberOfMessages stopWatch.ElapsedMilliseconds

        messages |> hasOrderPreserved output

    [<Property(Timeout=3000000)>]
    let ``All messages respect sequence number order into a sequence with not enought workers`` (messages:SequenceMessage<Message> list) = 
        let outbox = messageReceiver ()
        let receiveMessage = receiveMessage outbox
        let getMessages () = getMessages outbox
        let numberOfMessages = messages |> List.length |> int64

        let stopWatch = System.Diagnostics.Stopwatch.StartNew()
        messages 
        |> ActorModel.dispatch 100 receiveMessage
        |> Async.RunSynchronously

        let output = messages |> waitAllMessages getMessages 
        
        match stopWatch.ElapsedMilliseconds with
        | 0L -> printfn "no messages"
        | _ -> 
            printfn "messages %i in %ims" numberOfMessages stopWatch.ElapsedMilliseconds

        messages |> hasOrderPreserved output