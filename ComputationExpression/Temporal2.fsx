﻿type TimeSpan = System.TimeSpan

type DateTime = System.DateTime

type Period = 
    { startDate : DateTime
      endDate : DateTime }
    override this.ToString() = 
        let toString (date:DateTime) = date.ToString("yyyy/MM/dd")
        sprintf "[%s; %s[" (this.startDate |> toString) (this.endDate |> toString)

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
        | Some i -> seq { yield! state; yield i }
        | None -> state

    temporaries |> Seq.fold folder Seq.empty
    
let (=>) startDate endDate = 
    { startDate = startDate
      endDate = endDate }

let (:=) period value = { period=period; value=value }

//Test
let utcDate y m d = DateTime(y, m, d, 0, 0, 0, System.DateTimeKind.Utc)
let d2015 = utcDate 2015
let jan15 = d2015 1
let feb15 = d2015 2

let sort temporaries = temporaries |> Seq.sortBy (fun t -> t.period.startDate)

let defaultToNone temporaries = ()
    
    
let map f temporaries = temporaries |> Seq.map(fun t -> t.period := f t.value)

let apply tfs tvs = 
    let sortedv = tvs |> sort
    let apply tf = 
        sortedv
        |> view tf.period
        |> Seq.map(fun t -> { period=t.period; value=tf.value t.value })
    
    tfs 
    |> sort
    |> Seq.collect apply

let (<!>) = map
let (<*>) = apply

let map2 f temporaries = 
    temporaries
    |> Seq.map(fun t -> t.period := f t.value)

let print source = source |> Seq.iter (printfn "%A")

let availability close closeToDeparture price = (close, closeToDeparture, price)

availability
<!> [ jan15 2 => jan15 5 := false; jan15 5 => jan15 20 := true ]
<*> [ jan15 1 => jan15 2 := true; jan15 2 => jan15 19 := false ]
<*> [ jan15 2 => jan15 22 := 120m ]
|> print
