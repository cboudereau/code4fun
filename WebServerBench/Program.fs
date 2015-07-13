module Program

open Server

[<EntryPoint>]
let main args = 
    
    use host = new AsyncWebserver(asyncServer 20)
    printfn "press any keys to stop"
    System.Console.ReadKey() |> ignore
    
    0