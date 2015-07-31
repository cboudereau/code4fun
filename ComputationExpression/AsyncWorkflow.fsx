#load "Async.fs"
#load "AsyncStateMonad.fs"
#load "Temporal.fs"

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
