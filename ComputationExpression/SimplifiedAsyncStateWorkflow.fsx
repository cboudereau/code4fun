module AsyncStateMonad =
    type State<'a, 'b> = 
        | Success of 'a
        | Failure of 'b

    let rec bind f x =
        async {
            let! x' = x
            match x' with
            | Success v -> return! f v
            | Failure f -> return Failure f
        }

    let result x = async { return Success x }

    let failure f = async { return Failure f }

    type StateBuilder() = 
        member __.Bind(x, f) = x |> bind f
        member __.Return(x) = result x
        member __.ReturnFrom(x) = x
    
    let state = StateBuilder()


module Workflow = 

    open AsyncStateMonad

    let getRequest request = state { return request }

    let validate request = 
        state {
            let! request' = request
            if request' = "done" 
            then return! result true
            else return! failure "not done"
        }

    let run = "done" |> getRequest |> validate |> Async.RunSynchronously


module WorkflowSeq = 
    open AsyncStateMonad
    open Workflow

    let getRequests requests = requests |> Seq.map(fun r -> getRequest r)

    let validateAll requests = requests |> Seq.map(validate)

    let runAll = ["done"; "d";"done"] |> getRequests |> validateAll |> Async.Parallel |> Async.RunSynchronously