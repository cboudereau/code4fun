module Azure

open Microsoft.ServiceBus.Messaging
open Microsoft.WindowsAzure
open Microsoft.ServiceBus

let getBody (message : BrokeredMessage) = message.GetBody<string>()
let getSessionId (message : BrokeredMessage) = message.SessionId
let getSequenceNumber (message : BrokeredMessage) = message.SequenceNumber

let send (queue:QueueClient) message = 
            queue.Send message

let sendAll (queue:QueueClient) messages = 
    for message in messages do
        message |> send queue
                    
let createQueueListener address = 
    let connectionString = CloudConfigurationManager.GetSetting("Microsoft.ServiceBus.ConnectionString")
    let messagingFactory = MessagingFactory.CreateFromConnectionString(connectionString)
    messagingFactory.CreateQueueClient("test-session")

let createQueueSender address = 
    let connectionString = CloudConfigurationManager.GetSetting("Microsoft.ServiceBus.ConnectionString")
    let namespaceManager = NamespaceManager.CreateFromConnectionString(connectionString)
    if (namespaceManager.QueueExists(address)) then namespaceManager.DeleteQueue(address)
    let queueDescription = new QueueDescription(address)
    queueDescription.RequiresSession <- true
    namespaceManager.CreateQueue(queueDescription) |> ignore
    QueueClient.CreateFromConnectionString(connectionString, address)
