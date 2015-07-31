module Temporal =
    type DateTime = System.DateTime
    type HalfOpenPeriod = { startDate:DateTime; endDate:DateTime }
    type Temporary<'a> = { period: HalfOpenPeriod; value:'a }

    let jan15 d = DateTime(2015,1,d)

    let (==>) dateFrom dateTo = { startDate = dateFrom; endDate = dateTo }
    let (:=) period value = { period = period; value = value }

module Async = 
    let private toSeq computations = async { let! c = computations in return c |> Array.toSeq }
    let parrallel computations = computations |> Async.Parallel |> toSeq
    let collect computations = 
        let self i = i
        async {
            let! l = computations |> parrallel
            return l |> Seq.collect self
        }

module AsyncStateMonad =
    type State<'a, 'b> = 
        | Success of 'a
        | Failure of 'b
        | States of Async<State<'a, 'b>> seq

    let rec bind f x =
        async{
            let! x' = x
            match x' with
            | Success v -> return! f v
            | Failure f -> return Failure f
            | States states -> return States (states |> Seq.map(bind f))
        }

    let result x = async { return Success x }

    let foreach f l = async { return States (l |> Seq.map (f)) } 
    
    let fold fSuccess fFailure x =
        let folder acc i = seq { yield i; yield! acc } 
        let rec fold x = 
            async {
                let! x' = x
                match x' with
                | Success v -> return fSuccess v |> Seq.singleton
                | Failure f -> return fFailure f |> Seq.singleton
                | States s -> return! s |> Seq.map (fold) |> Seq.fold folder Seq.empty |> Async.collect
            }

        fold x
    
    let failure f = async { return Failure f }

    type StateBuilder() = 
        member __.Bind(x, f) = bind f x
        member __.Return(x) = result x
        member __.ReturnFrom(x) = x
        member __.For(l,f) = foreach f l
        member __.Yield(x) = result x
        member __.YieldFrom(x) = x
    
    let state = StateBuilder()

// Use Monad in inventory update context
open Temporal
open AsyncStateMonad

type UpdateStateFailure = 
    | ContractsMissed
    | ContractNotFound
    | ContractNotFoundOnPeriod
    | PartnerCommunicationFailure
    | ResponseParsingFailure

type InventoryUpdateResponse = { success:bool; content:string }

let getContracts flag = 
    state { 
        match flag with
        | true ->
            return 
                [ (1, [ jan15 1 ==> jan15 2 := Some "c11"; jan15 2 ==> jan15 10 := Some "c12" ])
                  (2, [ jan15 1 ==> jan15 2 := Some "c21"; jan15 2 ==> jan15 5 := Some "c22"; jan15 5 ==> jan15 10 := None ]) ] |> Map.ofSeq
        | _ -> return! failure ContractsMissed
    }

type Update = { name:string }

let getUpdates = 
    state {
        return
            [ (1, [ jan15 1 ==> jan15 2 := { name = "u11" }; jan15 2 ==> jan15 10 := { name = "u12" } ] )
              (2, [ jan15 1 ==> jan15 2 := { name = "u21" }; jan15 2 ==> jan15 5 := { name = "u22" }; jan15 5 ==> jan15 10 := { name = "u23" } ] )
              (3, [ jan15 1 ==> jan15 2 := { name = "u21" } ] ) ]
    }

let flatten (temporalContract, temporalUpdate) = 
    let firstContract = (temporalContract |> Seq.head).value
    temporalUpdate |> Seq.map(fun t -> t.period := (firstContract,t.value))

let dateToString (date:DateTime) = date.ToString("yyyyMMdd")

let matchContract contracts (id,temporalUpdate) = 
    state {
        match contracts |> Map.tryFind id with
        | None -> return! failure ContractNotFound
        | Some temporalContract -> return (temporalContract,temporalUpdate)
    }

let toRequest temporaryUpdateWithContract = 
    state {
        let (contract, update) = temporaryUpdateWithContract.value
        match contract with
        | Some c -> return sprintf "%s ==> %s := c(%s) u(%s)" (dateToString temporaryUpdateWithContract.period.startDate) (dateToString temporaryUpdateWithContract.period.endDate) c update.name
        | None -> return! failure ContractNotFoundOnPeriod
    }

let send flag request = 
    state {
        if flag 
        then return sprintf "sent ==> %s" request
        else return! failure PartnerCommunicationFailure
    }

let parseResponse flag response = 
    state{
        if flag
        then return sprintf "parsed ==> %s" response
        else return! failure ResponseParsingFailure
    }

let updateWorflow = 
    state{
        let! contracts = getContracts true
        let! updates = getUpdates
        for update in updates do
            let! (temporalContract, temporalUpdate) = matchContract contracts update
            for temporaryUpdateWithContract in flatten(temporalContract, temporalUpdate) do
                let! request =  temporaryUpdateWithContract |> toRequest
                let! response = request |> send true
                yield! response |> parseResponse true
    }

let toFailedResponse stateFailure = 
    let content stateFailure = 
        match stateFailure with
        | ContractsMissed -> "ContractsMissed"
        | ContractNotFound -> "ContractNotFound"
        | ContractNotFoundOnPeriod -> "ContractNotFoundOnPeriod"
        | PartnerCommunicationFailure -> "PartnerCommunicationFailure"
        | ResponseParsingFailure -> "ResponseParsingFailure"
    { success=false; content= content stateFailure }

let toSuccessResponse content = { success=true; content=content }

updateWorflow
|> AsyncStateMonad.fold toSuccessResponse toFailedResponse
|> Async.RunSynchronously 
|> Seq.toList
