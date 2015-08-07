module Async
    let toSeq computations = async { let! c = computations in return c |> Array.toSeq }
    let asParallel computations = computations |> Async.Parallel |> toSeq
    let private collect run computations = 
        let self i = i
        async {
            let! l = computations |> run
            return l |> Seq.collect self
        }

    let collectAsParallel computations = collect asParallel computations
