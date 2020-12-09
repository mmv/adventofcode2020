module Day09

open Utils

let hasPairThatSums x xs =
    Seq.allPairs xs xs
    |> Seq.exists (fun (a,b) -> x = a + b)

let findInvalidNumber preamble input =
    input
    |> Seq.map System.Numerics.BigInteger.Parse
    |> Seq.windowed (preamble + 1)
    |> Seq.filter (fun xs -> not <| hasPairThatSums (Seq.last xs) (Seq.take preamble xs))
    |> Seq.map Seq.last
    |> Seq.head

let headIsSum (xs: bigint seq) (x: bigint) =
    let headUntilX =
        xs
        |> Seq.scan (fun (i,sum) x -> (i+1,sum+x)) (0, bigint 0)
        |> Seq.skipWhile (snd >> ((>) x))
        |> Seq.head
    
    if (snd headUntilX) = x
    then Some(fst headUntilX)
    else None    

let rec findContiguousSum xs (x: bigint) =
    match headIsSum xs x with
    | Some(range) -> (Seq.take range >> Seq.min) xs, (Seq.take range >> Seq.max) xs
    | None -> findContiguousSum (Seq.tail xs) x

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
        |> Seq.toArray

    findContiguousSum input invalidNumber
    |> fun (a,b) -> a + b
    |> string