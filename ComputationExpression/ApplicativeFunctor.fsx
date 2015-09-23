module ApplicativeFunctor =

    type M<'a, 'b> = M of ('a -> 'b option)

    let run f o = 
        match o with
        | Some r -> f r
        | None -> None

    let bind f c = 
        let bind o = 
            let r = run f o
            run c r
        M bind
    
    let map (f:'a->'b) (M c:M<'a,'c>):M<'b, 'c> = 
        let map o = 
            let r = f o
            run c r
        M map
    
    let apply fO pO = 
        match fO, pO with
        | Some f, Some p -> f p |> Some
        | _ -> None

    let (<!>) = map
    let (<*>) = apply

