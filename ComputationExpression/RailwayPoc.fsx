type State<'a, 'b> = 
    | Success of 'a
    | Failure of 'b

let send request = 
    match request with
    | "s" -> Success "s"
    | v -> Failure v

let parse response =
    match response with
    | Success "s" -> Success true
    | Failure v | Success v -> Failure v

let parse2 response =
    match response with
    | "s" -> Success true
    | other -> Failure other

let (>>) a f = 
    match a with
    | Success a -> f a
    | Failure b -> Failure b

let c = send >> parse

c "i"