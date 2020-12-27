module Day15

open Utils

type MemSlot =
    | Once of int
    | Twice of int * int

let turn mem round last =
    let nextNum = 
        match Map.tryFind last mem with
        | Some(Twice(b,a)) -> b - a
        | Some(Once(a)) -> 0
        | None -> 0
    
    let nextMem =
        match Map.tryFind nextNum mem with
        | Some(Twice(b,a)) -> Map.add nextNum (Twice(round,b)) mem
        | Some(Once(a)) -> Map.add nextNum (Twice(round,a)) mem
        | None -> Map.add nextNum (Once(round)) mem
        
    (nextNum, nextMem)
    // |> tapv (printfn "%A %A %A" round last)


let runGame (xs: int seq) =
    let startingMem = 
        xs
        |> Seq.indexed
        |> Seq.map (fun (i,x) -> (x,Once(i+1)))
        |> Map.ofSeq

    let roundCount = (Map.count startingMem) + 1
    let last = xs |> Seq.last
    
    Seq.initInfinite ((+) roundCount)
    |> Seq.scan (fun (last,mem) x -> turn mem x last) (last,startingMem)

let solve rounds =
    let input = (readLines 15).[0].Split(',')

    input
    |> Seq.map int
    |> runGame
    |> Seq.take (rounds - input.Length + 1)
    |> Seq.last
    |> fst
    |> string

let solve1 () = solve 2020

let solve2 () = solve 3000000