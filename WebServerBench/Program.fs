module Program

open Server

[<EntryPoint>]
let main args = 
    
    use host = new AsyncWebserver(asyncServer 0)
    printfn "press any keys to stop"
    System.Console.ReadKey() |> ignore
    
    host |> ignore
        
    0