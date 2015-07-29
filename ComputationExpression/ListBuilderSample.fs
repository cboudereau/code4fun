module ListBuilderSample

type ListBuilder() =
    member this.Bind(m, f) = 
        m |> List.collect f

    member this.Zero() = 
        printfn "Zero"
        []

    member this.Return(x) = 
        printfn "Return an unwrapped %A as a list" x
        [x]

    member this.Yield(x) = 
        printfn "Yield an unwrapped %A as a list" x
        [x]
        
    member this.For(m,f) =
        printfn "For %A" m
        this.Bind(m,f)

// make an instance of the workflow                
let listbuilder = new ListBuilder()

listbuilder { 
    let! x = [1..3]
    let! y = [10;20;30]
    return x + y
} |> printfn "Result :%A"