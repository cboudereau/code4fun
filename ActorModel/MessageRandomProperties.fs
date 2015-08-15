module MessageRandomProperties
open FsCheck.Xunit
open MessageGen
open ActorModel

[<Arbitrary(typeof<RandomMessages>)>]
module MessageGenProperties = 
    
    [<Xunit.Fact>] let dummyTest() = Xunit.Assert.True(true)

    [<Property>]
    let ``Gen must generate message with unique id`` (messages:SequenceMessage<Message> list) =
        let uIds = messages |> List.map(fun m -> m.uid)
        uIds |> List.distinct = uIds

    [<Property>]
    let ``Gen must generate zero or multiple messages for same sequence`` (messages:SequenceMessage<Message> list) =
        messages 
        |> List.groupBy(fun m -> m.sequenceId)
        |> List.map(fun (_,l) -> l |> List.length)
        |> List.forall(fun c -> c > 0 || c > 1 || c > 2)
    
    [<Property>]
    let ``Sequence number must be unique per sequenceId in order to identify order preservation`` (messages:SequenceMessage<Message> list) =
        messages
        |> List.groupBy(fun m -> m.message.sequenceId)
        |> List.forall(fun (_,l) -> l |> List.distinctBy(fun m -> m.message.sequenceNumber) = l)