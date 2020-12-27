module Day22

open Utils
open FSharpx.Collections

let rec play xs ys =
    // printfn "1: %A" xs
    // printfn "2: %A" ys
    if Queue.length xs = 0
    then ys
    else if Queue.length ys = 0
    then xs
    else
        let xh,xt = xs.Uncons
        let yh,yt = ys.Uncons
        if xh > yh
        then play (xt.Conj(xh).Conj(yh)) yt
        else play xt (yt.Conj(yh).Conj(xh))

type Winner = Player1 | Player2

let rec recPlay xs ys prevXs prevYs =
    // printfn "1: %A" xs
    // printfn "2: %A" ys

    if Set.contains (xs |> Seq.toArray) prevXs && Set.contains (ys |> Seq.toArray) prevYs
    then
        // printfn "loop break"
        (Player1, xs)
    else if Queue.length xs = 0 then (Player2, ys)
    else if Queue.length ys = 0 then (Player1, xs)
    else
        let xh,xt = xs.Uncons
        let yh,yt = ys.Uncons
        let nextXs = Set.add (xs |> Seq.toArray) prevXs
        let nextYs = Set.add (ys |> Seq.toArray) prevYs
        if not (xh <= Queue.length xt && yh <= Queue.length yt)
        then
            // play as normal
            if xh > yh
            then recPlay (xt.Conj(xh).Conj(yh)) yt nextXs nextYs
            else recPlay xt (yt.Conj(yh).Conj(xh)) nextXs nextYs
        else
            // printfn "Recursing"
            // recurse
            match recPlay (xt |> Seq.take xh |> Queue.ofSeq) (yt |> Seq.take yh |> Queue.ofSeq) Set.empty Set.empty with
            | (Player1, _) -> recPlay (xt.Conj(xh).Conj(yh)) yt nextXs nextYs // (Set.add (xt |> Seq.toArray) prevXs) (Set.add (yt |> Seq.toArray) prevYs)
            | (Player2, _) -> recPlay xt (yt.Conj(yh).Conj(xh)) nextXs nextYs // (Set.add (xt |> Seq.toArray) prevXs) (Set.add (yt |> Seq.toArray) prevYs)


let parseInput lines =
    let decks = 
        batchSplit lines
        |> Seq.map (Seq.tail >> fun deck ->
            Seq.map int deck
            |> Queue.ofSeq
        )
        |> Seq.toArray
    
    decks.[0], decks.[1]

let solve1 () =
    let d1,d2 = readLines 22
                |> parseInput

    play d1 d2
    |> Seq.rev
    |> Seq.mapi (fun i x -> (i+1)*x)
    |> Seq.reduce (+)
    |> string
    

let solve2 () =
    let d1,d2 = readLines 22
                |> parseInput

    recPlay d1 d2 Set.empty Set.empty
    |> snd
    // |> tapv (Seq.map string >> Seq.reduce (fun a b -> a+","+b) >> printfn "%A")
    |> Seq.rev
    |> Seq.mapi (fun i x -> (bigint i+1I)*bigint x)
    |> Seq.reduce (+)
    |> string