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
let divideBy3 = divideBy 3
maybe{
    let! a = 12 |> divideBy3
    let! b = a |> divideBy 0
    return b
}

type State<'a, 'b> = 
    | Success of 'a
    | Failure of 'b

let divide bottom top = 
    if bottom = 0
    then Failure 0
    else Success (top/bottom)

type StateBuilder() = 
    member __.Bind(x, f) = 
        match x with
        | Success a -> f a
        | Failure n -> Failure n

    member __.Return(x) = Success x

let state = new StateBuilder();

state{
    let! a = 12 |> divide 0
    let! b = a |> divide 3
    return b
}







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