module Temporal = 
    type TimeSpan = System.TimeSpan
    
    type DateTime = System.DateTime
    
    type System.TimeSpan with
        static member Infinite = DateTime.MaxValue - DateTime.MinValue
        static member Never = TimeSpan.Zero
    
    let (|Infinite|Never|Value|) timeSpan = 
        match timeSpan with
        | t when t >= TimeSpan.Infinite -> Infinite
        | t when t <= TimeSpan.Never -> Never
        | t -> Value t
    
    type M<'a> = M of(Period -> 'a option)

    and Period = 
        { startDate : DateTime
          endDate : DateTime }

        static member sort f p1 p2 = 
            if p1.startDate <= p2.startDate then f p1 p2
            else f p2 p1

        static member contiguous f =
            let contiguous p1 p2 =
                if p1.endDate >= p2.startDate then Some (f p1 p2)
                else None
            Period.sort contiguous

        static member intersect f = 
            let intersect p1 p2 =
                { startDate = max p1.startDate p2.startDate
                  endDate = min p1.endDate p2.endDate }
                |> f
            Period.contiguous intersect
        
        static member union f =
            let union p1 p2 =
                { startDate = min p1.startDate p2.startDate
                  endDate = max p1.endDate p2.endDate}
                |> f
            Period.contiguous union
    
    module Temporary =
        type Temporary<'a> = 
            { period : Period
              value : 'a }

        let valueEquals f v1 v2 = 
            match f, v1=v2 with
            | Some f', true -> (f' v1)
            | _ -> None

        let merge f t1 t2 = 
            let build period value = Some { period=period; value=value }
            
            let buildUnionPeriod = Period.union build t1.period t2.period
            match valueEquals buildUnionPeriod t1.value t2.value with
            | Some v -> Some (f v)
            | None -> None
       
    open Temporary

    let (=>) startDate endDate = 
        { startDate = startDate
          endDate = endDate }
    
    let (:=) period value = 
        { period = period
          value = value }
    
    type Temporal<'a> = 
        | Temporal of 'a Temporary seq

open Temporal

let utcDate y m d = DateTime(y, m, d, 0, 0, 0, System.DateTimeKind.Utc)
let d2015 = utcDate 2015
let jan15 = d2015 1
let feb15 = d2015 2

[ jan15 1 => jan15 20 := "Hello" ]
