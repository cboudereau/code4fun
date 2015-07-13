module Server

open System
open System.Net
open System.Text
open System.Threading

type Webserver(server) = 
    let listener = new HttpListener()
    let cancellationTokenSource = new CancellationTokenSource()
    
    do 
        listener.Prefixes.Add("http://+:8080/")
        listener.IgnoreWriteExceptions <- true
        listener.Start()
        ThreadPool.QueueUserWorkItem(WaitCallback(fun _ -> server cancellationTokenSource.Token listener)) 
        |> ignore

    member __.CancellationTokenSource with get() = cancellationTokenSource
    
    interface IDisposable with
        member __.Dispose() = 
            cancellationTokenSource.Cancel()
            listener.Stop()

type AsyncWebserver(server) = 
    let listener = new HttpListener()
    let cancellationTokenSource = new CancellationTokenSource()
    
    do 
        listener.Prefixes.Add("http://+:8080/")
        listener.IgnoreWriteExceptions <- true
        listener.Start()
        Async.Start(listener |> server cancellationTokenSource.Token, cancellationTokenSource.Token)

    member __.CancellationTokenSource with get() = cancellationTokenSource
    
    interface IDisposable with
        member __.Dispose() = 
            cancellationTokenSource.Cancel()
            listener.Stop()

let message = "Echo"
let content = Encoding.UTF8.GetBytes(message)

let writer thinkTime (context : HttpListenerContext) = 
    Thread.Sleep(thinkTime:int)
    let response = context.Response
    response.StatusCode <- 200
    response.ContentLength64 <- content.LongLength
    do response.OutputStream.Write(content, 0, content.Length)
    response.OutputStream.Close()
    response.Close()

let asyncWriter thinkTime (context : HttpListenerContext) = 
    async { 
        do! Async.Sleep(thinkTime)
        let response = context.Response
        response.StatusCode <- 200
        response.ContentLength64 <- content.LongLength
        do! response.OutputStream.AsyncWrite(content, 0, content.Length)
        response.OutputStream.Close()
        response.Close()
    }

open System.Threading

let server thinkTime (cancellationToken : CancellationToken) (listener : HttpListener) = 
    let rec internalServer() = 
        if(cancellationToken.IsCancellationRequested) then ()
        let context = listener.GetContext()
        ThreadPool.QueueUserWorkItem(WaitCallback(fun _ -> context |> writer thinkTime)) |> ignore
        internalServer()
    internalServer()

let asyncServer thinkTime (cancellationToken : CancellationToken) (listener : HttpListener) = 
    let rec internalAsyncServer() = 
        async { 
            let! context = listener.GetContextAsync() |> Async.AwaitTask
            Async.Start(context |> asyncWriter thinkTime, cancellationToken)
            return! internalAsyncServer()
        }
    internalAsyncServer()
