type OptionSample() =
    member __.Bind(x, f) = 
        match x with
        | Some x' -> f x'
        | None -> None
    member __.Return(x) = Some x

let option = OptionSample()

let parseInt v = 
    match System.Int32.TryParse(v) with
    | true, i -> Some i
    | _ -> None

let display s1 s2 = 
    printfn "%A, %A" s1 s2

let result a b = 
    option{

        let! i = parseInt a
        printfn "%i" i
        let! j = parseInt b

        if i = 0 then return i + j
    }


let v = result "23" "a"

display v.Value v.Value