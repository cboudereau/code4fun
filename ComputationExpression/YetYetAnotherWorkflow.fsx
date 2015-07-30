type Async = 
    static member toSeq computations = async { let! c = computations in return c |> Array.toSeq }
    static member collect computations = async { let! c = computations in return c |> Seq.collect(fun i -> i) }


type State<'a> = 
    | Success of 'a
    | Failure
    | ContractNotFound
    | ContractNotFoundOnPeriod

let bind f x =
    async {
        let! x' = x
        match x' with
        | Success v -> return! f v
        | Failure -> return Failure
        | ContractNotFound -> return ContractNotFound
        | ContractNotFoundOnPeriod -> return ContractNotFoundOnPeriod }

let result x = async { return Success x }

type StateBuilder() = 
    member __.Bind(x, f) = bind f x
    member __.Return(x) = result x
    member __.ReturnFrom(x) = x
    member __.For((l:seq<'a>), (m:'a -> Async<State<'b>>)) : seq<Async<State<'b>>> = l |> Seq.map(m)
    member __.Yield(x) = result x
    member __.YieldFrom(x) = x
    member __.Zero() = 
        printfn "Zero"
        result []

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

let getContracts = 
    state { 
        return 
            [ (1, [ jan15 1 ==> jan15 2 := Some "c11"; jan15 2 ==> jan15 10 := Some "c12" ] |> Temporal.toTemporal)
              (2, [ jan15 1 ==> jan15 2 := Some "c21"; jan15 2 ==> jan15 5 := Some "c22"; jan15 5 ==> jan15 10 := None ] |> Temporal.toTemporal) ] |> Map.ofSeq
    }

type Update = { name:string }

let getUpdates = 
    [ (1, [ jan15 1 ==> jan15 2 := { name = "u11" }; jan15 2 ==> jan15 10 := { name = "u12" } ] |> Temporal.toTemporal )
      (2, [ jan15 1 ==> jan15 2 := { name = "u21" }; jan15 2 ==> jan15 5 := { name = "u22" }; jan15 5 ==> jan15 10 := { name = "u23" } ] |> Temporal.toTemporal )
      (3, [ jan15 1 ==> jan15 2 := { name = "u21" } ] |> Temporal.toTemporal ) ] |> List.toSeq

let getTemporal contracts (id,u) = 
    match contracts |> Map.tryFind id with
    | Some temporalContract -> 
        //Here flatten with real function, here it is a poc without temporal implem
        state { 
            let c = (temporalContract |> Seq.head).value
            match c with
            | None -> return! async { return ContractNotFoundOnPeriod }
            | Some c' -> return! async { return u |> Seq.map(fun t -> t.period := (c',t.value)) |> Temporal.toTemporal |> Success }
        }
    | None -> async { return ContractNotFound }

let toRequest temporaryUpdate =
    let s (dateTime:DateTime) = dateTime.ToString("yyyyMMdd")
    let (contract, update) = temporaryUpdate.value
    sprintf "%s ==> %s := contract=%s;update=%A" (s temporaryUpdate.period.startDate)  (s temporaryUpdate.period.endDate) contract update

let r = 
    state{
        for u in getUpdates do 
            let! contracts = getContracts
            let! temporals = getTemporal contracts u
            yield temporals |> Seq.map toRequest |> Seq.toList
    }

let g = 
    r
    |> Async.Parallel
    |> Async.toSeq
    |> Async.RunSynchronously
    |> Seq.toList
