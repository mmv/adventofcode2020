module Day23

open Utils
open System.Collections.Immutable

// first attempt, using lists, good enough for part 1
let play cups =
    let current = List.head cups
    let hand = List.take 3 (List.tail cups)
    let rest = List.skip 4 cups
    let xmin = List.min rest
    let xmax = List.max rest
    
    let target =
        let rec findTarget t =
            let t' = if t < xmin then xmax else t
            if List.contains t' rest then t'
            else findTarget (t' - 1)
        findTarget (current - 1)
    
    let newList =
        let rec buildList =
            function
            | [] -> [current]
            | x::xs when x = target -> target::hand@(buildList xs)
            | x::xs -> x::(buildList xs)
        buildList rest
    
    newList

// second attempt using arrays
// has better performance but not enough for part2
let play' cups =
    let current = Seq.head cups
    let hand = Seq.take 3 (Seq.tail cups)
    let rest = Array.skip 4 cups
    let xmin = Array.min rest
    let xmax = Array.max rest
    
    let target =
        let rec findTarget t =
            let t' = if t < xmin then xmax else t
            if Array.contains t' rest then t'
            else findTarget (t' - 1)
        findTarget (current - 1)
    
    let newList xs =
        seq {
            for x in xs do
                yield x
                if x = target then
                    yield! hand
            yield current
        } |> Seq.toArray

    newList rest

// same as previous but using ImmutableList
// worse performance
let play'' (cups: ImmutableList<int>) =
    let current = Seq.head cups
    let hand = Seq.take 3 (Seq.tail cups)
    let rest = cups.RemoveRange(0,4)
    let xmin = 1
    let xmax = 1000000
    
    let target =
        let rec findTarget t =
            let t' = if t < xmin then xmax else t
            match rest.IndexOf(t') with
            | -1 -> findTarget (t' - 1)
            | x -> x
        findTarget (current - 1)
    
    let newList =
        rest.GetRange(0,target+1)
            .AddRange(hand)
            .AddRange(rest.GetRange(target+1, rest.Count - target - 1))

    newList

// I wanted to use a linked list but wasn't understanding how could I
// change it and keep everything immutable, until I remembered that a
// linked list can be easily simulated by a Map where the value is the
// next element for that key. Immutable maps are easy enough to use
// and this works with acceptable performance for part2.
let play''' cups current xmin xmax =
    let hand = { 1..2 } |> Seq.scan (fun n _ -> Map.find n cups) cups.[current] |> Seq.toArray

    let cupsWithoutHand = cups |> Map.add current (cups.[hand.[2]])

    let target =
        let rec findTarget t =
            let t' = if t < xmin then xmax else t
            if Array.contains t' hand
            then findTarget (t' - 1)
            else t'
        findTarget (current - 1)

    let afterTarget = Map.find target cupsWithoutHand

    let newList =
        cupsWithoutHand
        |> Map.add target hand.[0]
        |> Map.add hand.[2] afterTarget

    newList, newList.[current]
    



let printer result =
    let p = Seq.findIndex ((=) 1) result
    Seq.concat [
        Seq.skip (p + 1) result
        Seq.take p result
    ]
    |> Seq.map string
    |> Seq.reduce (+)

let solve1 () = 
    let input = readLines 23
                |> Array.exactlyOne
                |> Seq.toList
                |> List.map (int >> (fun c -> c - int '0'))

    let result = 
        { 1..100 }
        |> Seq.scan (fun x _ -> play x) input
        |> tap (printfn "%A")
        |> Seq.last

    printer result


let solve2 () =
    let cupCount = 1000000
    let input = readLines 23
                |> Array.exactlyOne
                |> Seq.toList
                |> Seq.map (int >> (fun c -> c - int '0'))

    let starting = Seq.head input

    let list =  { 10..cupCount }
                |> Seq.append input
                |> Seq.pairwise
                |> Map.ofSeq
                |> Map.add cupCount starting

    let rlist, rcurrent = 
        { 1..10000000 }
        |> Seq.scan (fun (cups,current) _ -> play''' cups current 1 cupCount) (list, starting)
        |> Seq.last

    printfn "%A" rlist.[1]
    printfn "%A" rlist.[rlist.[1]]

    (bigint rlist.[1]) * (bigint rlist.[rlist.[1]])
    |> string
    

