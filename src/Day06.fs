module Day06

open Utils

let solve1 () =
    readLines 6
    |> batchSplit
    |> Seq.sumBy (Seq.collect id >> set >> Seq.length)
    |> string

let solve2 () =

    readLines 6
    |> batchSplit
    |> Seq.sumBy (Seq.map set                   // seq<string> -> seq<set<char>>
                  >> Seq.reduce (Set.intersect) // -> set<char>
                  >> Set.count)
    |> string