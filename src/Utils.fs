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