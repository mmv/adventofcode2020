module Day05

open Utils

let binSearch test (input) =
    
    let rec binMapper xs =
        match xs with
        | [] -> 0
        | [x] -> test x
        | x::xs -> (binMapper xs) * 2 + (test x)
    
    input |> Seq.rev |> Seq.toList |> binMapper


let seatNum input =
    let test x =
        match x with
        | 'F' -> 0
        | 'B' -> 1
        | 'R' -> 1
        | 'L' -> 0
        | _ -> failwith $"bad value {x}"
    
    binSearch test input

let solve1 () =
    readLines 5
    |> Seq.map seatNum
    |> Seq.max
    |> string

let solve2 () =
    
    let assignedSeats = readLines 5
                        |> Seq.map seatNum
                        |> set
    
    assignedSeats
    |> Seq.filter (fun x -> assignedSeats.Contains(x + 2) && not <| assignedSeats.Contains(x+1))
    |> Seq.exactlyOne
    |> (+) 1
    |> string