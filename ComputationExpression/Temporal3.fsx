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

let (=>) startDate endDate = 
    { startDate = startDate
      endDate = endDate }

type M<'a> = M of (Period -> ('a * Period) option)

let (:=) period value = 
    let find p = 
        match Period.intersect period p with
        | Some inter -> Some (value, inter)
        | _ -> None
    M find

let runM (M f) period = f period

let mapM f mx = 
    let find period = 
        match runM mx period with
        | Some (value, p) -> Some (f value, p)
        | None -> None
    M find

let mapMSeq f source = source |> Seq.map (mapM f)

let map2M f m1 m2 = 
    let find period = 
        match runM m1 period with
        | Some (v1,p1) -> 
            match runM m2 p1 with
            | Some (v2,p2) -> Some (f v1 v2,p2)
            | None -> None
        | _ -> None
    M find

let applyM mf mx = 
    let find period =
        match runM mf period with
        | Some (f, fp) ->
            match runM mx fp with
            | Some (x, xp) -> Some (f x, xp)
            | None -> None
        | None -> None
    M find

let applyMSeq mfs source = failwith "not yet implemented"

let (<!>) = mapMSeq
let (<*>) = applyMSeq

//Test
let utcDate y m d = DateTime(y, m, d, 0, 0, 0, System.DateTimeKind.Utc)
let d2015 = utcDate 2015
let jan15 = d2015 1
let feb15 = d2015 2

let availability close closeToDeparture price = (close, closeToDeparture, price)

availability
<!> [ jan15 2 => jan15 20 := false ]
//<*> [ jan15 1 => jan15 10 := true ]
//<*> [ jan15 20 => jan15 22 := 120m ]
