module Day10

open Utils

let solve1 () =
    let diffs =
        readLines 10
        |> Seq.map int
        |> Seq.append [ 0 ]
        |> Seq.sort
        |> Seq.pairwise
        |> Seq.map (fun (a,b) -> b - a)
        |> Seq.cache
    
    (
        (diffs |> Seq.filter ((=) 1) |> Seq.length)
        *
        (diffs |> Seq.filter ((=) 3) |> Seq.length |> (+) 1)
    )
    |> string

let solve2 () =
    let input =
        readLines 10
        |> Seq.map int

    let xs =
        input |> Seq.append [ 0; 3 + Seq.max input ]
        |> Seq.sort
        |> Seq.toList

    let inline mapinc key increment start (m: Map<'a,'b>) =
        m.Change(key, (Option.defaultValue start) >> (+) increment >> Some)
    
    // consume adapters and mark pathCounts with how many ways to get
    // to that value
    // return the count for the final adapter (desired output)
    let rec fillPathCounts xs (pathCounts: Map<int,bigint>) =
        let thisCount = pathCounts.[List.head xs]
        match xs with
        | [] -> failwith "unexpected end of list"
        | [x] -> thisCount
        | x::xs ->
            pathCounts
            |> mapinc (x+1) (bigint 0) thisCount
            |> mapinc (x+2) (bigint 0) thisCount
            |> mapinc (x+3) (bigint 0) thisCount
            |> fillPathCounts xs
    
    fillPathCounts xs (Map [(0, bigint 1)])
    |> string    
