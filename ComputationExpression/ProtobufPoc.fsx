#r @"..\packages\protobuf-net.2.0.0.668\lib\net40\protobuf-net.dll"

open ProtoBuf

open System
open System.IO

//Issue when type is not defined and the generalization process occurs, Protobuf infert type to object and throw : No serializer defined for type: System.Object
//Use SkipConstructor in order to avoid constructor call. I don't know the impact for performance??
[<ProtoContract(SkipConstructor = true); Serializable>]
type CustomerName(firstName:string, lastName:string) = 
    let mutable firstName = firstName
    let mutable lastName = lastName
    
    [<ProtoMember(1)>]
    member __.FirstName with get () = firstName and set value = firstName <- value
    
    [<ProtoMember(2)>]
    member __.LastName with get () = lastName and set value = lastName <- value

//This call give the proto file which describe the schema data used.
Serializer.GetProto<CustomerName>()



let customer = CustomerName("clem","boud")


let fileStream = File.OpenWrite("d:\dump.proto")

Serializer.Serialize(fileStream, customer)

fileStream.Dispose()

let stream = new MemoryStream()
Serializer.Serialize(stream, customer)

stream.Flush()
stream.Seek(0L, SeekOrigin.Begin)

Serializer.Deserialize<CustomerName>(stream)

