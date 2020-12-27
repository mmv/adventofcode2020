module Day13

open Utils
open FSharpx.Collections

let readInput () =
    let lines = readLines 13
    let departTime = int lines.[0]
    let busTimes =
        lines.[1].Split(",")
        |> Seq.filter ((<>) "x")
        |> Seq.map int
    
    departTime, busTimes

let readMatchMinutes () =
    let lines = readLines 13
    lines.[1].Split(",")
    |> Seq.mapi (fun idx x -> idx,x)
    |> Seq.filter (snd >> (<>) "x")
    |> Seq.map fst


let solve1 () =
    let t, xs = readInput ()
    let closestBus =
        xs
        |> Seq.sortBy (fun x -> ((t / x) + 1) * x)
        |> Seq.head
    
    printfn "%A" closestBus
    let waitMins = (((t / closestBus) + 1) * closestBus) - t

    closestBus * waitMins
    |> string

type ItemEnumerator = int64 * int64
let nextItem ((c, x): ItemEnumerator) = (c+x, x)
let itemValue ((c, _): ItemEnumerator) = c

let nmod a b =
    let m = a % b
    if m >= 0I then m else b+m
 
let extEuclidean (a : bigint) (b : bigint) =
    let rec inner (rprev, sprev, tprev) (r, s, t) =
        let q = rprev / r
        let r' = rprev - q*r
        let s' = sprev - q*s
        let t' = tprev - q*t
        if r' = 0I then (rprev, sprev, tprev)
        else inner (r, s, t) (r', s', t')
    inner (a, 1I, 0I) (b, 0I, 1I)
 
/// chinese remainder theorem
/// b_i (mod n_i)
let chinRemainder ns bs =
    let getMs a b = extEuclidean a b |> (fun (_,_,m) -> m)
    let nproduct = ns |> Seq.reduce (*)
    let nquotients = ns |> Seq.map (fun n -> nproduct/n)
    let ms = Seq.map2 getMs ns nquotients
    let azs = Seq.map3 (fun a b c -> a*b*c) bs ms nquotients
    nmod (Seq.sum azs) nproduct

let solve2 () =
    let matchMinutes = readInput () |> snd
    let timeDeltas =
        let lines = readLines 13
        lines.[1].Split(",")
        |> Seq.rev
        |> Seq.index
        |> Seq.filter (fun (i,x) -> x <> "x") |> Seq.map fst

    let result =
        chinRemainder
            (matchMinutes |> Seq.map bigint |> Seq.rev)
            (timeDeltas   |> Seq.map bigint)

    result - bigint (Seq.max timeDeltas)
    |> string

    
