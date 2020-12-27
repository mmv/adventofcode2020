module Day25

open Utils

type Subject = Card | Door

let loopIter (n: int) (v: int) = bigint v * bigint n % 20201227I |> int

let loop card door =
    
    Seq.initInfinite ignore
    |> Seq.scan (fun s _ -> loopIter 7 s) 1
    |> Seq.indexed
    |> Seq.choose (fun (i,x) -> if x = card then Some(i,Card)
                                elif x = door then Some(i,Door)
                                else None)
    |> Seq.head
    |> tapv (printfn "%A")


let solve1 () =
    let card, door = readLines 25 |> Array.head |> pairSplit " " |> pairMap int
    match loop card door with
    | n,Card -> { 1..(n) } |> Seq.fold (fun v _ -> loopIter door v ) 1
    | n,Door -> { 1..(n) } |> Seq.fold (fun v _ -> loopIter card v ) 1
    |> string
    

let solve2 () = ""