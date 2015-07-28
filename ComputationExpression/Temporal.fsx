
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

type Event<'a> = 
    { duration : TimeSpan
      value : 'a }

type Moment<'a> = 
    | Start of DateTime
    | Moment of Event<'a>

let (||)

let add moment moments = 
    match moment with
    |