type TimeSpan = System.TimeSpan

type DateTime = System.DateTime

type Period = 
    { startDate : DateTime
      endDate : DateTime }
    override this.ToString() = 
        let toString (date:DateTime) = date.ToString("yyyy/MM/dd")
        sprintf "[%s; %s[" (this.startDate |> toString) (this.endDate |> toString)

    static member always = { startDate=DateTime.MinValue; endDate=DateTime.MaxValue }

    static member empty = { startDate = DateTime.MinValue; endDate = DateTime.MinValue }

    static member sort f p1 p2 = 
        if p1.startDate <= p2.startDate then f p1 p2
        else f p2 p1

    static member intersect p1 p2 = 
        let intersect p1 p2 = 
            if p1.endDate >= p2.startDate 
            then 
                { startDate = max p1.startDate p2.startDate
                  endDate = min p1.endDate p2.endDate }
                |> Some
            else None
        Period.sort intersect p1 p2

    static member union p1 p2 = 
        let union p1 p2 = 
            if p1.endDate >= p2.startDate
            then 
                { startDate = min p1.startDate p2.startDate
                  endDate = max p1.endDate p2.endDate }
                |> Some
            else None
        Period.sort union p1 p2

type Temporary<'a> = 
    { period : Period
      value : 'a }
    override this.ToString() = sprintf "%O = %O" this.period this.value

module Temporary =
    let empty<'a> = { period=Period.empty; value=Option<'a>.None }
    let union t1 t2 = 
        match t1.value = t2.value, Period.union t1.period t2.period with
        | false, _ | _, None -> None
        | true, Some p -> Some { period=p; value=t1.value }

open Temporary
let view period temporaries = 
    let intersect t = 
        match Period.intersect t.period period with
        | Some i -> Some { period=i; value= t.value }
        | _ -> None
    
    let folder state t =
        match intersect t with
        | Some i -> seq { yield! state; yield i }
        | None -> state

    temporaries |> Seq.fold folder Seq.empty

let (=>) startDate endDate = 
    { startDate = startDate
      endDate = endDate }

let (:=) period value = { period=period; value=value }

let sort temporaries = temporaries |> Seq.sortBy (fun t -> t.period.startDate)

let defaultToNone temporaries = 
    let toOption t = { period=t.period; value=Some t.value }
    let rec defaultToNone temporaries = 
        seq{
            match temporaries with
            | [] -> yield! Seq.empty
            | [t] -> yield { period=t.period;value=Some t.value }
            | t1::t2::tail ->
                match Period.intersect t1.period t2.period with
                | Some _ -> yield t1 |> toOption; yield! defaultToNone (t2::tail)
                | None -> 
                    yield t1 |> toOption
                    yield { period={ startDate=t1.period.endDate; endDate=t2.period.startDate }; value = None }
                    yield! defaultToNone (t2::tail)
        }

    let toAlways l = 
        seq{
            yield {period={startDate=DateTime.MinValue; endDate=DateTime.MinValue};value=None }
            yield! (l |> sort |> Seq.map toOption)
            yield {period={startDate=DateTime.MaxValue; endDate=DateTime.MaxValue};value=None } 
        }
    temporaries |> toAlways |> Seq.toList |> defaultToNone
    
let map f temporaries = temporaries |> defaultToNone |> Seq.map(fun t -> t.period := f t.value)

let apply tfs tvs = 
    let sortedv = tvs |> defaultToNone
    let apply tf = 
        sortedv
        |> view tf.period
        |> Seq.map(fun t -> { period=t.period; value=tf.value t.value })
    
    tfs 
    |> Seq.collect apply

let merge temporaries = 
    let rec merge temporaries = 
        seq{
            match temporaries with
            | t1::t2::tail ->
                match Temporary.union t1 t2 with
                | Some u -> yield! merge (u::tail)
                | None -> yield t1; yield! merge (t2::tail)
            | [] -> yield! Seq.empty
            | [t] -> yield t
        }
    temporaries |> Seq.toList |> merge

let (<!>) = map
let (<*>) = apply




//Test
let utcDate y m d = DateTime(y, m, d, 0, 0, 0, System.DateTimeKind.Utc)
let d2015 = utcDate 2015
let jan15 = d2015 1
let feb15 = d2015 2

let print source = source |> Seq.iter (printfn "%O")

//defaultToNone
[ jan15 10 => jan15 20 := "Hello"
  jan15 22 => jan15 23 := "Toto" ] 
|> defaultToNone 
|> print

let availability close closeToDeparture price = (close, closeToDeparture, price)

availability
<!> [ jan15 2 => jan15 5 := false; jan15 5 => jan15 20 := true ]
<*> [ jan15 2 => jan15 19 := false; jan15 1 => jan15 2 := true ]
<*> [ jan15 2 => jan15 22 := 120m ]
|> print


//merge
[ jan15 10 => jan15 20 := "Hello"
  jan15 12 => jan15 14 := "Hello"
  jan15 23 => jan15 24 := "Tutu"
  jan15 24 => jan15 26 := "Tutu"
  jan15 26 => jan15 28 := "Tutu" ] 
|> defaultToNone
//|> merge
|> print