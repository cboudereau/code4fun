module Async
    let private toSeq computations = async { let! c = computations in return c |> Array.toSeq }
    let parrallel computations = computations |> Async.Parallel |> toSeq
    let collect computations = 
        let self i = i
        async {
            let! l = computations |> parrallel
            return l |> Seq.collect self
        }