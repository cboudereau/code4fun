module Bench

open Xunit
open FsUnit.Xunit

open System
open System.Net
open System.IO
open System.Diagnostics
open System.Threading.Tasks

open Server
open System.Threading

let asyncRequest i = 
    async {
        let request = WebRequest.CreateHttp("http://localhost:8080")
        let! response = request.AsyncGetResponse()
        
        use responseStream = response.GetResponseStream()
        use reader = new StreamReader(responseStream)
        let! result = reader.ReadToEndAsync() |> Async.AwaitTask
        result |> should equal message
    }

let request i = 
    let request = WebRequest.CreateHttp("http://localhost:8080")
    let response = request.GetResponse().GetResponseStream()
    use reader = new StreamReader(response)
    let result = reader.ReadToEnd()
    result |> should equal message

[<Measure>] type rq
[<Measure>] type s

let maxRequest = 100000m<rq>
let thinkTime = 0
let asyncServerWithThinkTime = asyncServer thinkTime
let serverWithThinkTime = server thinkTime
let elapsed (stopwatch:Stopwatch) = LanguagePrimitives.DecimalWithMeasure<s> (decimal stopwatch.ElapsedMilliseconds / 1000m)

let callAsTplSync maxRequest = 
    let stopwatch = Stopwatch.StartNew()
    Parallel.ForEach([1 .. maxRequest |> decimal |> int], request) |> ignore
    printfn "%A" (decimal maxRequest / elapsed stopwatch)

[<Fact(Timeout = 600000)>]
let ``just call server only for bench purpose``()=  
    callAsTplSync maxRequest

[<Fact(Timeout = 600000)>]
let ``tpl client async server``() = 
    ServicePointManager.DefaultConnectionLimit <- 50

    use host = new AsyncWebserver(asyncServerWithThinkTime)
    callAsTplSync maxRequest
    host |> ignore

[<Fact(Timeout = 600000)>]
let ``tpl client tpl server``()=
    ServicePointManager.DefaultConnectionLimit <- 50
    use host = new Webserver(serverWithThinkTime)    
    callAsTplSync maxRequest
    host |> ignore

let asyncCallAsParallel maxRequest = 
    let stopwatch = Stopwatch.StartNew()

    [1 .. maxRequest |> decimal |> int]
    |> List.map(asyncRequest)
    |> Async.Parallel
    |> Async.Ignore
    |> Async.RunSynchronously
    
    let result = (maxRequest / elapsed stopwatch) 

    printfn "%A rq/s" result
    
[<Fact(Timeout = 600000)>]
let ``async client async server``() = 
    ServicePointManager.DefaultConnectionLimit <- 50

    use host = new AsyncWebserver(asyncServerWithThinkTime)    
    asyncCallAsParallel maxRequest
    host |> ignore

[<Fact(Timeout = 600000)>]
let ``async client tpl server``() = 
    ServicePointManager.DefaultConnectionLimit <- 50

    use host = new Webserver(serverWithThinkTime) 
    asyncCallAsParallel maxRequest
    host |> ignore

//[<Fact>]
let ``smoke test : server async``() = 
    use host = new AsyncWebserver(asyncServerWithThinkTime)    
    
    let stopwatch = Stopwatch.StartNew()

    [1 .. 2]
    |> List.map(asyncRequest)
    |> Async.Parallel
    |> Async.Ignore
    |> Async.RunSynchronously
    
    Console.WriteLine(stopwatch.ElapsedMilliseconds)