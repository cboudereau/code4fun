#load "Async.fs"

type State<'a> = 
    | Success of 'a
    | Failure

let bind (f:'a -> Async<State<'b>>) (x:Async<State<'a>>) =
    async {
        let! v = x
        match v with
        | Success v' -> return! f v'
        | Failure -> return Failure
    }

type StateBuilder() = 
    member __.Bind(x,f) = bind f x
    member __.Return(x) = async { return Success x }
    member __.ReturnFrom(x) = x
    member __.Yield(x) = async { return Success x }
    member __.YieldFrom(x) = x
    member __.Combine(x1,x2) = 
        seq {
            yield x1
            yield! x2
        }
    member __.Delay(f) = f()
    member __.For(x, f) = x |> Seq.map f

let state = StateBuilder()

let toRequest flag requests = 
    match flag with
    | "s" -> requests |> Seq.map(fun v -> async { return v |> Success })
    | _ -> async { return Failure } |> Seq.singleton

let send flag request  = 
    match flag with
    | true -> async { return request |> Success } |> Seq.singleton
    | _ -> async { return Failure } |> Seq.singleton

let parse flag response = 
    match flag with
    | 1 -> async { return response |> Success }
    | _ -> async { return Failure }

let r0:Async<State<string>> = state { return! parse 1 "hello" }
let r0':Async<State<string>> = 
    state {  
        let! v = parse 1 "hello"
        return v
    }

let r1:seq<Async<State<string>>> = 
    state{ 
        return! send true "hello"
    }

let r2:seq<Async<State<string>>> = 
    state {
        for s in send true "hello" do
            yield! s
    }

let r3:seq<Async<State<string>>> = 
    state { 
        for s in send true "hello" do
            let! s' = s
            yield! parse 1 s' }

let r4:seq<Async<State<string>>> = 
        send true "Hello"
        |> Seq.collect(fun c -> state { yield! send true "hello" })

let failure() = async { return Failure }
let result r = async { return Success r }

let sendWithContract flag requests  = 
    state {
        let! requests' = requests
        match flag with
        | true -> return requests'
        | _ -> return! failure()
    }
    
let parseSingle flag r = 
    match flag with
    | 1 -> 
        state { 
            return r
        }
    | _ -> state { return! failure() }
   
let rec recParseSends flag responses = 
    state {
        match responses with
        | "hello" :: tail -> 
            yield "hello"
            yield! recParseSends flag tail
        | _ -> yield! (failure() |> Seq.singleton)
    }

let parseSends flag responses = 
    match flag with
    | 1 -> 
        state {
            let! responses' = responses
            return recParseSends flag (responses' |> Seq.toList)
        }
    | _ -> state { yield! failure() }

let parseWithState flag responses = 
    
    //May make for??
    state {
        let! responses' = responses
        return responses' |> Seq.map (parseSingle flag)
    }

let r5 = 
    let getContract () = result (["hello"; "bad"] |> List.toSeq)
    let r51 = 
        state {
            let sends = sendWithContract true (getContract()) 
            return! sends |> parseSends 1
        }
    r51

let result5 = r5 |> Async.RunSynchronously
match result5 with
| Success x -> x |> Async.asParallel |> Async.RunSynchronously |> Seq.toList
| Failure -> []
