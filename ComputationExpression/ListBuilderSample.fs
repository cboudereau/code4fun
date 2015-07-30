type ListBuilder() =
    member this.Bind(m, f) = 
        m |> List.map f

    member this.Return(x) = 
        printfn "Return an unwrapped %A as a list" x
        [x]

// make an instance of the workflow                
let listbuilder = new ListBuilder()

listbuilder { 
    let! x = [1..3]
    let! y = [10;20;30]
    return x + y
} |> printfn "Result :%A"
