type DateTime = System.DateTime
type HalfOpenPeriod = { startDate:DateTime; endDate:DateTime }
type Temporary<'a> = { period: HalfOpenPeriod; value:'a }

let jan15 d = DateTime(2015,1,d)

let (==>) dateFrom dateTo = { startDate = dateFrom; endDate = dateTo }
let (:=) period value = { period = period; value = value }

type Async = 
    static member private toSeq computations = async { let! c = computations in return c |> Array.toSeq }
    static member ParrallelSeq computations = computations |> Async.Parallel |> Async.toSeq
    static member Collect computations = 
        let self i = i
        async {
            let! l = computations |> Async.ParrallelSeq
            return l |> Seq.collect self
        }

type UpdateStateFailure = 
    | ContractsMissed
    | ContractNotFound
    | ContractNotFoundOnPeriod
    | PartnerCommunicationFailure
    | ResponseParsingFailure

type InventoryUpdateResponse = { success:bool; content:string }

type State<'a, 'b> = 
    | Success of 'a
    | Failure of 'b
    | States of Async<State<'a, 'b>> seq

module AsyncStateMonad =
    let rec bind f x =
        async{
            let! x' = x
            match x' with
            | Success v -> return! f v
            | Failure f -> return Failure f
            | States states -> return States (states |> Seq.map(bind f))
        }

    let result x = async { return Success x }

    let for' f l = async { return States (l |> Seq.map (f)) } 

    let fold fSuccess fFailure x =
        let rec fold x = 
            async {
                let! x' = x
                match x' with
                | Success v -> return fSuccess v |> Seq.singleton
                | Failure f -> return fFailure f |> Seq.singleton
                | States s -> return! Seq.fold (fun acc i -> seq { yield i; yield! acc }) Seq.empty (s |> Seq.map (fold)) |> Async.Collect
            }

        fold x

type StateBuilder() = 
    member __.Bind(x, f) = AsyncStateMonad.bind f x
    member __.Return(x) = AsyncStateMonad.result x
    member __.ReturnFrom(x) = x
    member __.For(l,f) = AsyncStateMonad.for' f l
    member __.Yield(x) = AsyncStateMonad.result x
    member __.YieldFrom(x) = x

let state = StateBuilder()

let failure f = async { return Failure f }

let success v = async { return Success v }

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
    state{
        return
            [ (1, [ jan15 1 ==> jan15 2 := { name = "u11" }; jan15 2 ==> jan15 10 := { name = "u12" } ] )
              (2, [ jan15 1 ==> jan15 2 := { name = "u21" }; jan15 2 ==> jan15 5 := { name = "u22" }; jan15 5 ==> jan15 10 := { name = "u23" } ] )
              (3, [ jan15 1 ==> jan15 2 := { name = "u21" } ] ) ]
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

AsyncStateMonad.fold toSuccessResponse toFailedResponse stateResponse |> Async.RunSynchronously |> Seq.toList
