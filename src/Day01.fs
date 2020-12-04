module Day01

open Utils

let solve1 () =

    let inputs =
        readLines 1
        |> Seq.map int
        |> set
    
    let matched =
        inputs
        |> Seq.filter ((-) 2020 >> inputs.Contains)
        |> Seq.head

    $"{matched * (2020 - matched)}"

let solve2 () =
    let inputs =
        readLines 1
        |> Seq.map int
        |> set

    seq {
        for a in inputs do
            for b in inputs do
                for c in inputs do
                    if a+b+c = 2020 then yield (a,b,c)
    }
    |> Seq.head
    |> fun (a,b,c) -> a*b*c
    |> string
