
type Key = Key of string

type Directory = Directory of string

type Contract = Contract of string

module Client = 
    let getContract (Key key) =
        let message = sprintf "get contract %A" key
        printfn "%s" message
        Contract message

module FileStorage = 
    let tryFind (Directory directory) (Key key) = 
        if System.IO.Directory.Exists directory |> not then None
        else
            let founds = System.IO.Directory.EnumerateFiles(directory, key, System.IO.SearchOption.TopDirectoryOnly)

            if founds |> Seq.isEmpty then None
            else 
                System.IO.File.ReadAllText (founds |> Seq.exactlyOne) |> Some
        
    let update (Directory directory) (Key key) value = 
        printfn "%s" (System.IO.Path.GetFullPath(directory))
        
        if System.IO.Directory.Exists directory |> not then System.IO.Directory.CreateDirectory(directory) |> ignore

        let path = System.IO.Path.Combine(directory, key)

        System.IO.File.WriteAllText(path, value)

    let flush (Directory directory) = if System.IO.Directory.Exists(directory) then System.IO.Directory.Delete(directory, true)

let cacheDir = Directory "_cache"

let agent = 
    FileStorage.flush cacheDir
    MailboxProcessor.Start <| fun inbox ->
        let rec listen () = 
            async {
                let! (key, reply) = inbox.Receive()
                match FileStorage.tryFind cacheDir key with
                | Some value -> value |> Contract |> reply
                | None -> 
                    let (Contract value) = Client.getContract key
                    value |> FileStorage.update cacheDir key
                    value |> Contract |> reply
                    
                do! listen ()
            }
        listen ()

let sendOne _ = (agent.PostAndAsyncReply <| fun channel -> Key "1", channel.Reply)

[1 .. 100] |> Seq.map sendOne |> Async.Parallel |> Async.RunSynchronously

