module Result

let inline orElse f g x = 
    match f x with
    | Ok y -> Ok y
    | Error e1 ->
        match g x with
        | Error e2 -> Error (e1,e2)
        | Ok z -> Ok z

let inline andThen f g x = 
    match f x with
    | Ok y -> 
        match g x with
        | Ok z -> Ok (y,z)
        | Error e -> Error e
    | Error e -> Error e

let inline (<|>) f g = orElse f g
let inline (.>>.) f g = andThen f g

let inline isError x = match x with Error _ -> true | Ok _ -> false
let inline isOk x = match x with Ok _ -> true | Error  _ -> false
let inline errorf x = Printf.ksprintf Error x

let inline ofOptiond error = function Some x -> Ok x | None -> error () |> Error
let inline ofOption error = function Some x -> Ok x | None -> error |> Error
let inline ofOptionf error = function Some x -> Ok x | None -> Printf.ksprintf Error error
let inline toOption x = match x with Ok y -> Some y | Error _ -> None

let ofSeq source = source |> Seq.fold (fun s x -> s |> Result.bind (fun s' -> x |> Result.map (fun x' -> seq { yield! s'; yield x' }))) (Ok Seq.empty)
let toSeq x = 
    match x with
    | Ok x' -> x' |> Seq.map Ok
    | Error e -> Error e |> Seq.singleton

let (<!>) f x = Result.map f x
let (<*>) f x = f |> Result.bind(fun f' -> x |> Result.map (fun x' -> f' x'))
