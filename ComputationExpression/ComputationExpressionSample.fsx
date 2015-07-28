type LoggerBuiler(service) = 
    let log kind p = printfn "[%s : %s] : %A" service kind p

    member this.Bind(x, f) = 
        log "arg" x
        f x

    member this.Return(x) = 
        log "result" x
        x

let logAddition = new LoggerBuiler("Add");

logAddition{
    let! x = 5
    let! y = 10
    return x + y
}

type Opt<'a> = 
    | S of 'a
    | N

let divideBy bottom top = 
    if bottom = 0
    then N
    else S (top/bottom)

type MaybeBuilder() = 
    member __.Bind(x, f) = 
        match x with
        | S a -> f a
        | N -> N

    member __.Return(x) = S x

let maybe = new MaybeBuilder();

maybe{
    let! a = 12 |> divideBy 3
    let! b = a |> divideBy 0
    return b
}

type Temporary = {startDate:System.DateTime; endDate: System.DateTime}

type OrElseBuilder() =
    member this.ReturnFrom(x) = x
    member this.Combine (a,b) = 
        match a with
        | Some _ -> a  // a succeeds -- use it
        | None -> 
            printfn "debug %A" b
            b    // a fails -- use b instead
    member this.Delay(f) = f()

let orElse = new OrElseBuilder()

let map1 = [ ("1","One"); ("2","Two") ] |> Map.ofList
let map2 = [ ("A","Alice"); ("B","Bob") ] |> Map.ofList
let map3 = [ ("CA","California"); ("NY","New York") ] |> Map.ofList

let multiLookup key = 
    orElse {
        return! map1.TryFind key
        return! map2.TryFind key
        return! map3.TryFind key }

multiLookup "A" |> printfn "Result for A is %A" 
multiLookup "CA" |> printfn "Result for CA is %A" 
multiLookup "X" |> printfn "Result for X is %A"