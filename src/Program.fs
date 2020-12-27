// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

[<EntryPoint>]
let main argv =

    let solvers = [|
        Day01.solve1 ; Day01.solve2
        Day02.solve1 ; Day02.solve2
        Day03.solve1 ; Day03.solve2
        Day04.solve1 ; Day04.solve2
        Day05.solve1 ; Day05.solve2
        Day06.solve1 ; Day06.solve2
        Day07.solve1 ; Day07.solve2
        Day08.solve1 ; Day08.solve2
        Day09.solve1 ; Day09.solve2
        Day10.solve1 ; Day10.solve2
        Day11.solve1 ; Day11.solve2
        Day12.solve1 ; Day12.solve2
        Day13.solve1 ; Day13.solve2
        Day14.solve1 ; Day14.solve2
        Day15.solve1 ; Day15.solve2
        Day16.solve1 ; Day16.solve2
        Day17.solve1 ; Day17.solve2
        Day18.solve1 ; Day18.solve2
        Day19.solve1 ; Day19.solve2
        Day20.solve1 ; Day20.solve2
        Day21.solve1 ; Day21.solve2
        Day22.solve1 ; Day22.solve2
        Day23.solve1 ; Day23.solve2
        Day24.solve1 ; Day24.solve2
        Day25.solve1
    |]

    printfn "%s" ((Seq.last solvers)())
    0 // return an integer exit code