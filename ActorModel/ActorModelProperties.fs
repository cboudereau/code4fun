module ActorModelProperties

open FsCheck
open FsCheck.Xunit

open ActorModel
open Availpro

[<Xunit.Fact>]
let ``dummy test``() = Xunit.Assert.True(true)

[<Arbitrary(typeof<TestData.RandomMessage>)>]
module Properties = 

    [<Property>]
    let ``validate message random`` (data:SequenceMessage<Message> list) = 
                
        printfn "%A" data
