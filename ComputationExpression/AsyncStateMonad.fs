module AsyncStateMonad
    type State<'a, 'b> = 
        | Success of 'a
        | Failure of 'b
        | States of Async<State<'a, 'b>> seq

    let rec bind f x =
        async {
            let! x' = x
            match x' with
            | Success v -> return! f v
            | Failure f -> return Failure f
            | States states -> return States (states |> Seq.map(bind f))
        }

    let result x = async { return Success x }

    let foreach f l = async { return States (l |> Seq.map (f)) } 
    
    let fold fSuccess fFailure x =
        let folder (acc:#seq<_>) i = 
            seq { 
                let a = acc.GetEnumerator()
                while a.MoveNext() do yield a.Current
                yield i  } 

        let rec fold x = 
            async {
                let! x' = x
                match x' with
                | Success v -> return fSuccess v |> Seq.singleton
                | Failure f -> return fFailure f |> Seq.singleton
                | States s -> return! s |> Seq.map (fold) |> Seq.fold folder Seq.empty |> Async.collectAsParallel
            }

        fold x
    
    let failure f = async { return Failure f }

    type StateBuilder() = 
        member __.Bind(x, f) = bind f x
        member __.Return(x) = result x
        member __.ReturnFrom(x) = x
        member __.For(l,f) = foreach f l
        member __.Yield(x) = result x
        member __.YieldFrom(x) = x
    
    let state = StateBuilder()
