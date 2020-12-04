module Day03

open Utils

let TREE = '#'

let makeTreeChecker () =
    let inputData = readLines 3
    let baseInput =
        inputData
        |> Seq.mapi (fun rnum line ->
            line |> Seq.mapi (fun cnum c -> ((rnum, cnum), c = TREE))
        )
        |> Seq.collect id
        |> Map.ofSeq

    let rowCount = Array.length inputData
    let colCount = Seq.length << Array.head <| inputData

    let checker row col =
        baseInput.[(row % rowCount, col % colCount)]
    
    (rowCount, colCount, checker)

let solve1 () =
    let (rowCount, colCount, checker) = makeTreeChecker ()
    let rowStep = 1
    let colStep = 3
    
    Seq.initInfinite (fun i -> (i * rowStep, i * colStep))
    |> Seq.takeWhile (fun (r,_) -> r < rowCount)
    |> Seq.map (fun (r,c) -> checker r c)
    |> Seq.filter id
    |> Seq.length
    |> string

let solve2 () =
    let (rowCount, colCount, checker) = makeTreeChecker ()
    
    let stepChecks = [|
        (1,1)
        (1,3)
        (1,5)
        (1,7)
        (2,1)
    |]

    stepChecks
    |> Seq.map(fun (rowStep, colStep) ->
        Seq.initInfinite (fun i -> (i * rowStep, i * colStep))
        |> Seq.takeWhile (fun (r,_) -> r < rowCount)
        |> Seq.map (fun (r,c) -> checker r c)
        |> Seq.filter id
        |> Seq.length
        |> bigint
    )
    |> Seq.reduce (*)
    |> string