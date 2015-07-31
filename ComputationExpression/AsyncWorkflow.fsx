type Async = 
    static member toList computations = async { let! c = computations in return c |> Array.toList }
    static member ParrallelList computations = computations |> Async.Parallel |> Async.toList

type StateFailure = 
    | ContractsMissed
    | ContractNotFound
    | ContractNotFoundOnPeriod
    | PartnerCommunicationFailure
    | ResponseParsingFailure

type InventoryUpdateResponse = { success:bool; content:string }

let inventoryUpdateResponseFailure stateFailure = 
    let content stateFailure = 
        match stateFailure with
        | ContractsMissed -> "ContractsMissed"
        | ContractNotFound -> "ContractNotFound"
        | ContractNotFoundOnPeriod -> "ContractNotFoundOnPeriod"
        | PartnerCommunicationFailure -> "PartnerCommunicationFailure"
        | ResponseParsingFailure -> "ResponseParsingFailure"
    { success=false; content= content stateFailure }

let inventoryUpdateResponseSuccess content = { success=true; content=content }

type State<'a> = 
    | Success of 'a
    | Failure of StateFailure
    | States of Async<State<'a>> seq

//let rec bind (f:'a -> Async<State<'b>>) (x:Async<State<'a>>) : Async<State<'b>> =
module AsyncState =
    let rec bind f x =
        async{
            let! x' = x
            match x' with
            | Success v -> return! f v
            | Failure f -> return Failure f
            | States states -> return States (states |> Seq.map(bind f))
        }

    let result x = async { return Success x }

    let run x = 
        let rec fold responses x =
            async {
                let! x' = x
                match x' with
                | Success v -> return (v |> inventoryUpdateResponseSuccess) :: responses
                | Failure f ->  return (f |> inventoryUpdateResponseFailure) :: responses
                | States states ->
                    return!
                        async {
                            let! statesFold = states |> Seq.map(fun i -> fold responses i) |> Async.ParrallelList
                            return statesFold |> List.collect(fun i -> i)
                        }
            }
        fold [] x

type StateBuilder() = 
    member __.Bind(x, f) = AsyncState.bind f x
    member __.Return(x) = AsyncState.result x
    member __.ReturnFrom(x) = x
    member __.For(l,f) = async { return States (l |> Seq.map (f)) }
    member __.Yield(x) = AsyncState.result x
    member __.YieldFrom(x) = x

let state = StateBuilder()

type DateTime = System.DateTime
type HalfOpenPeriod = { startDate:DateTime; endDate:DateTime }
type Temporary<'a> = { period: HalfOpenPeriod; value:'a }
type Temporal<'a> = Temporary<'a> seq

module Temporal = 
    let toTemporal t : Temporal<'a> = t

let jan15 d = DateTime(2015,1,d)

let (==>) dateFrom dateTo = { startDate = dateFrom; endDate = dateTo }
let (:=) period value = { period = period; value = value }

type Contracts = Map<int, Temporal<string option>>
let toContracts (t:Contracts) = t

let failure f = async { return Failure f }

let success v = async { return Success v }

let getContracts flag = 
    state { 
        match flag with
        | true ->
            return 
                [ (1, [ jan15 1 ==> jan15 2 := Some "c11"; jan15 2 ==> jan15 10 := Some "c12" ] |> Temporal.toTemporal)
                  (2, [ jan15 1 ==> jan15 2 := Some "c21"; jan15 2 ==> jan15 5 := Some "c22"; jan15 5 ==> jan15 10 := None ] |> Temporal.toTemporal) ] |> Map.ofSeq |> toContracts
        | _ -> return! failure ContractsMissed
    }

type Update = { name:string }

let getUpdates = 
    state{
        return
            [ (1, [ jan15 1 ==> jan15 2 := { name = "u11" }; jan15 2 ==> jan15 10 := { name = "u12" } ] |> Temporal.toTemporal )
              (2, [ jan15 1 ==> jan15 2 := { name = "u21" }; jan15 2 ==> jan15 5 := { name = "u22" }; jan15 5 ==> jan15 10 := { name = "u23" } ] |> Temporal.toTemporal )
              (3, [ jan15 1 ==> jan15 2 := { name = "u21" } ] |> Temporal.toTemporal ) ]
    }

let flatten (temporalContract, temporalUpdate) = 
    let firstContract = (temporalContract |> Seq.head).value
    temporalUpdate |> Seq.map(fun t -> t.period := (firstContract,t.value))

let dateToString (date:DateTime) = date.ToString("yyyyMMdd")

let matchContracts contracts (id,temporalUpdate) = 
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

let stateResponse = 
    state{
        let! contracts = getContracts true
        let! updates = getUpdates
        for update in updates do
            let! (temporalContract, temporalUpdate) = matchContracts contracts update
            for temporaryUpdateWithContract in flatten(temporalContract, temporalUpdate) do
                let! request =  temporaryUpdateWithContract |> toRequest
                let! response = request |> send true
                yield! response |> parseResponse true
    }

let r = AsyncState.run stateResponse |> Async.RunSynchronously
