type State<'a> = 
    | Success of 'a
    | Failure

let bind (f:'a -> State<'b>) (x:State<'a>) =
    match x with
    | Success v -> f v
    | Failure -> Failure

type StateBuilder() = 
    member __.Bind(x,f) = bind f x
    member __.Return(x) = Success x
    member __.ReturnFrom(x) = x
    member __.Yield(x) = Success x
    member __.YieldFrom(x) = x
    member __.For(x, f) = x |> Seq.map (bind f)
    member __.Zero() = Success ()

let state = StateBuilder()

let toRequest flag request = 
    match flag with
    | "s" -> Success request
    | _ -> Failure

let sendRequest flag request = 
    match flag with
    | true -> async { return request } |> Success
    | _ -> Failure

let parseRequest flag asyncResponse = 
    match flag with
    | true -> 
        async {  
            return! asyncResponse
        } |> Success
    | _ -> Failure

let sendRequests flag request = [request] |> Seq.map(sendRequest flag)

let getOneRequest flag asyncRequest = 
    async {
        let! request = asyncRequest
        match flag with
        | true -> return Success request
        | _ -> return Failure
    }

let getMRequests flag request = [request] |> Seq.map (getOneRequest flag)

let toAsync r = async { return r }

let r2:seq<State<Async<string>>> = 
    state {
        for s in sendRequests true "Hello" do
        yield s
    }

let r1:State<Async<string>> = 
    state {
        let! r11 = toRequest "s" "Hello"
        let! r12 = sendRequest true r11
        return! parseRequest true r12

    }
let r0:State<string> = state { return! toRequest "s" "hello" }
