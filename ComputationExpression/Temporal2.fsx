type TimeSpan = System.TimeSpan

type DateTime = System.DateTime

type Period = 
    { startDate : DateTime
      endDate : DateTime }
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

let (=>) startDate endDate = 
    { startDate = startDate
      endDate = endDate }

let (:=) period value = { period=period; value=value }

//Test
let utcDate y m d = DateTime(y, m, d, 0, 0, 0, System.DateTimeKind.Utc)
let d2015 = utcDate 2015
let jan15 = d2015 1
let feb15 = d2015 2

let defaultToNone temporaries = 
    let minStartDate = temporaries |> Seq.map(fun t -> t.period.startDate) |> Seq.min
    let maxEndDate = temporaries |> Seq.map(fun t -> t.period.endDate) |> Seq.max
     
    seq{
        yield DateTime.MinValue => minStartDate := None
        yield! temporaries |> Seq.map(fun t -> t.period := Some t.value)
        yield maxEndDate => DateTime.MaxValue := None
    }

let map f temporaries = 
    temporaries
    |> Seq.map(fun t -> t.period := f t.value)
    |> defaultToNone

let combine tv tf = 
    match Period.intersect tf.period tv.period, tf.value, tv.value with
    | Some period, Some f, Some v -> { period=period; value= f v |> Some }
    | _ -> { period=tv.period; value=None }

let combineN tfs tv = tfs |> List.map (combine tv)

let apply (first:Temporary<('a->'b) option> seq) second = 
    let sort temporaries = temporaries |> Seq.sortBy(fun t -> t.period.startDate) 
    let temporariesf = first |> sort |> Seq.toList
    let temporariesv = second |> sort |> defaultToNone |> Seq.toList
    
    temporariesv |> List.collect (combineN temporariesf)

let (<!>) = map
let (<*>) = apply

type M<'a> = M of (Period -> 'a option)



let map2 f temporaries = 
    temporaries
    |> Seq.map(fun t -> t.period := f t.value)

let availability close closeToDeparture price = (close, closeToDeparture, price)

let result = 
    availability
    <!> [ jan15 2 => jan15 20 := false ]
    <*> [ jan15 1 => jan15 10 := true ]
    <*> [ jan15 20 => jan15 22 := 120m ]
result |> List.map(fun t -> t.value)