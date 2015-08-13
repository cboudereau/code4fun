open Microsoft.FSharp.Control

type Actor<'a> = MailboxProcessor<'a>

let actor = 
    Actor.Start <| fun inbox -> 
        let rec listen() = 
            async {
                let! (replyChannel, message) = inbox.Receive()
                replyChannel message
                do! listen()
            }
        listen()


let messages = 
    [ (1, "H1", "Message1")
      (2, "H1", "Message2")
      (2, "H2", "Message1")
      (2, "H1", "Message3") ]

let post message = actor.PostAndAsyncReply <| fun replyChannel -> (replyChannel.Reply, message)

post "Hello" |> Async.RunSynchronously