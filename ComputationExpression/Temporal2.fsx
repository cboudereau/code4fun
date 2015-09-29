﻿type TimeSpan = System.TimeSpan

type DateTime = System.DateTime

type Period = 
    { startDate : DateTime
      endDate : DateTime }
    override this.ToString() = 
        let toString (date:DateTime) = date.ToString("yyyy/MM/dd")
        sprintf "[%s; %s[" (this.startDate |> toString) (this.endDate |> toString)
    
    static member duration p = p.endDate - p.startDate

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

let view period temporaries = 
    let intersect t = 
        match Period.intersect t.period period with
        | Some i -> Some { period=i; value= t.value }
        | _ -> None
    
    let folder state t =
        match intersect t with
        | Some i -> 
            seq { 
                yield! state
                if i.period.startDate > t.period.startDate
                then 
                yield i }
        | None -> state

    temporaries |> Seq.fold folder Seq.empty

let (=>) startDate endDate = 
    { startDate = startDate
      endDate = endDate }

let (:=) period value = { period=period; value=value }

let sort temporaries = temporaries |> Seq.sortBy (fun t -> t.period.startDate)

let split length temporaries = 
    let rec split t = 
        seq{
            if t.period |> Period.duration <= length then yield t
            else
                let next = t.period.startDate + length
                yield { t with period = { t.period with endDate = next } }
                yield! split { t with period = { t.period with startDate = next } }
        }
    temporaries
    |> Seq.collect split

let defaultToNone temporaries = 
    let option t = { period=t.period; value=Some t.value }    
    
    let it i = i

    let folder state current = 
        let defaulted = 
            match state with
            | None -> option current |> Seq.singleton
            | Some previous -> 
                match Period.intersect previous.period current.period  with
                | Some _ -> seq { yield current |> option; }
                | None -> 
                    seq{
                        yield { period={startDate=previous.period.endDate; endDate=current.period.startDate};value=None }
                        yield option current
                    }
        defaulted, Some current
    
    temporaries
    |> Seq.mapFold folder None
    |> fst
    |> Seq.collect it

let merge temporaries = 

    let union t1 t2 = 
        match t1.value = t2.value, Period.union t1.period t2.period with
        | false, _ | _, None -> None
        | true, Some p -> Some { period=p; value=t1.value }

    let rec merge temporaries = 
        seq{
            match temporaries with
            | t1::t2::tail ->
                match union t1 t2 with
                | Some u -> yield! merge (u::tail)
                | None -> yield t1; yield! merge (t2::tail)
            | [] -> yield! Seq.empty
            | [t] -> yield t
        }
    temporaries |> Seq.toList |> merge

let map f temporaries = 
    temporaries 
    |> sort
    |> defaultToNone 
    |> merge
    |> Seq.map(fun t -> t.period := f t.value)

let apply tfs tvs = 
    let sortedv = tvs |> sort |> defaultToNone |> merge
    let apply tf = 
        sortedv
        |> view tf.period
        |> Seq.map(fun t -> { period=t.period; value=tf.value t.value })
    
    tfs 
    |> Seq.collect apply

let (<!>) = map
let (<*>) = apply

//Test
let utcDate y m d = DateTime(y, m, d, 0, 0, 0, System.DateTimeKind.Utc)
let d2015 = utcDate 2015
let jan15 = d2015 1
let feb15 = d2015 2

let print source = source |> Seq.iter (printfn "%O")

//merge
[ jan15 10 => jan15 20 := "Hello"
  jan15 12 => jan15 14 := "Hello"
  jan15 23 => jan15 24 := "Tutu"
  jan15 24 => jan15 25 := "Tutu"
  jan15 26 => jan15 28 := "Tutu" ] 
//|> defaultToNone
|> merge
|> print

//merge with defaultToNone (without overlap)
[ jan15 10 => jan15 12 := "Hello"
  jan15 12 => jan15 14 := "Hello"
  jan15 23 => jan15 24 := "Tutu"
  jan15 24 => jan15 25 := "Tutu"
  jan15 26 => jan15 28 := "Tutu" ] 
|> defaultToNone
|> merge
|> print

//split
[ jan15 10 => jan15 22 := "Hello" ]
|> split (TimeSpan.FromDays(5.))
|> print


let availability close closeToDeparture price = (close, closeToDeparture, price)

let contiguousSample = 
    availability
    <!> [ jan15 2 => jan15 5 := false; jan15 5 => jan15 20 := true ]
    <*> [ jan15 2 => jan15 19 := false; jan15 1 => jan15 2 := true ]
    <*> [ jan15 2 => jan15 22 := 120m ]

contiguousSample |> print

let mergedSample = 
    availability
    <!> [ jan15 2 => jan15 5 := false
          jan15 5 => jan15 7 := true
          jan15 7 => jan15 20 := true ]
    <*> [ jan15 2 => jan15 19 := false; jan15 1 => jan15 2 := true ]
    <*> [ jan15 2 => jan15 22 := 120m ]

mergedSample |> print

contiguousSample |> Seq.toList = (mergedSample |> Seq.toList)


availability
<!> [ jan15 4 => jan15 5 := false; jan15 5 => jan15 20 := true ]
<*> [ jan15 2 => jan15 19 := false; jan15 19 => jan15 23 := true ]
<*> [ jan15 1 => jan15 22 := 120m ]
|> print

//defaultToNone
[ jan15 10 => jan15 20 := "Hello"
  jan15 22 => jan15 23 := "Toto" ] 
|> defaultToNone 
|> print
