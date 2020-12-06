module Utils

open System.IO

let readLines (day: int) =
    let file = $"Inputs/{day}"
    File.ReadAllLines file

let pairSplit (c: string) (s: string) =
    match s.Split(c) with
    | [| a; b |] -> (a,b)
    | _ -> failwith $"didn't split well '{s}' '{c}'"

let isBetween a b x =
    a <= x && x <= b


let batchSplit (lines: string seq) =
    let batchNums = lines |> Seq.scan (fun s i -> if i = "" then s+1 else s) 0
    lines
    |> Seq.zip batchNums
    |> Seq.groupBy (fun (batch,line) -> batch)
    |> Seq.map (fun (key,(g)) ->
        g |> Seq.map (fun (_,b) -> b)
          |> Seq.filter (fun x -> x <> ""))