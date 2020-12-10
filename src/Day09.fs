module Day09

open Utils
open System.Collections.Immutable

let hasPairThatSums x xs =
    Seq.allPairs xs xs
    |> Seq.exists (fun (a,b) -> x = a + b)

let findInvalidNumber preamble input =
    input
    |> Seq.map bigint.Parse
    |> Seq.windowed (preamble + 1)
    |> Seq.filter (fun xs -> not <| hasPairThatSums (Seq.last xs) (Seq.take preamble xs))
    |> Seq.map Seq.last
    |> Seq.head

let rec findContiguousSum (xs: bigint list) (q: ImmutableQueue<bigint>) acc x =
    match acc - x with
    | a when a > bigint 0 -> let qhead = q.Peek ()
                             let qtail = q.Dequeue ()
                             findContiguousSum xs (qtail) (acc - qhead) x
    | a when a < bigint 0 -> let xhead = List.head xs
                             let xtail = List.tail xs
                             findContiguousSum xtail (q.Enqueue xhead) (acc + xhead) x
    | _ -> (Seq.min q), (Seq.max q)

let solve1 () =

    let preamble = 25

    readLines 9
    |> findInvalidNumber preamble
    |> string

let solve2 () =

    let preamble = 25
    
    let invalidNumber = 
        readLines 9
        |> findInvalidNumber preamble

    let input = 
        readLines 9
        |> Seq.map System.Numerics.BigInteger.Parse
        |> Seq.toList

    findContiguousSum input (ImmutableQueue.Empty) (bigint 0) invalidNumber
    |> fun (a,b) -> a + b
    |> string