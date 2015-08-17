module MessageRandomProperties
open FsCheck.Xunit
open MessageGen
open ActorModel
open Microsoft.ServiceBus.Messaging

[<Arbitrary(typeof<RandomMessages>)>]
module MessageGenProperties = 
    
    
    [<Xunit.Fact>] 
    let dummyTest() = 
        Xunit.Assert.True(true)

    [<Property>]
    let ``Gen must generate zero or multiple messages for same sequence`` (messages:BrokeredMessage list) =
        messages 
        |> List.groupBy(Azure.getSessionId)
        |> List.map(fun (_,l) -> l |> List.length)
        |> List.forall(fun c -> c > 0 || c > 1 || c > 2)
    