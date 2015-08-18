module Azure

open Microsoft.ServiceBus.Messaging
open Microsoft.WindowsAzure
open Microsoft.ServiceBus

[<Literal>]
let number = "Number"
let getBody (message : BrokeredMessage) = message.GetBody<string>()
let getSessionId (message : BrokeredMessage) = message.SessionId
let getProperty<'a> name (message:BrokeredMessage) = message.Properties.[name] :?> 'a
let getNumber (message:BrokeredMessage) = message |> getProperty<string> number

let send (queue:QueueClient) message = message |> queue.Send

let sendAll (queue:QueueClient) messages = 
    messages 
    |> List.map (send queue) 

let acceptMessageSession sessionId (queue:QueueClient) = queue.AcceptMessageSessionAsync(sessionId:string) |> Async.AwaitTask

let peek (queue:QueueClient) = queue.PeekAsync() |> Async.AwaitTask
 
let closeSession (messageSession:MessageSession) = messageSession.CloseAsync() |> Async.AwaitTask

let receiveMessage (messageSession:MessageSession) = messageSession.ReceiveAsync() |> Async.AwaitTask

let completeMessage (message:BrokeredMessage) = message.CompleteAsync() |> Async.AwaitTask

let connectionString = CloudConfigurationManager.GetSetting("Microsoft.ServiceBus.ConnectionString")
                 
let createQueueListener address = 
    let messagingFactory = MessagingFactory.CreateFromConnectionString(connectionString)
    messagingFactory.CreateQueueClient(address)

let deleteQueue address = 
    let namespaceManager = NamespaceManager.CreateFromConnectionString(connectionString)
    if (namespaceManager.QueueExists(address)) then namespaceManager.DeleteQueue(address)

let createQueueSender address = 
    let namespaceManager = NamespaceManager.CreateFromConnectionString(connectionString)
    address |> deleteQueue
    let queueDescription = new QueueDescription(address)
    queueDescription.RequiresSession <- true
    namespaceManager.CreateQueue(queueDescription) |> ignore
    QueueClient.CreateFromConnectionString(connectionString, address)
