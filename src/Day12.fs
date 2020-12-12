module Day12

open Utils

let parseInstruction (s: string) =
    s.[0], int (s.Substring(1))

let dirToAz d = match d with | 'N' -> 0 | 'E' -> 90 | 'S' -> 180 | 'W' -> 270 | _ -> failwith "unexpected"
let azToDir a = "NESW".[((a+360) % 360) / 90]

let rec moveShip xs (n,e) direction =
    let move direction =
        match direction with
          | 'N',v -> (n+v,e)
          | 'S',v -> (n-v,e)
          | 'E',v -> (n,e+v)
          | 'W',v -> (n,e-v)
          | _ -> (n,e)

    match xs with
    | [] -> (n,e)
    | x::xs -> 
        let newPosition = move (if (fst x) = 'F' then (direction, snd x) else x)
        let newDirection =
            match x with
            | 'L',v -> direction |> dirToAz |> (fun a -> a - v) |> azToDir
            | 'R',v -> direction |> dirToAz |> (fun a -> a + v) |> azToDir
            | _ -> direction
        
        moveShip xs newPosition newDirection


let rec moveWaypoint xs (n,e) (sn,se) =
    let rotate a = match (a+360)%360 with
                   | 0 -> (n,e)
                   | 90 -> (-e,n)
                   | 180 -> (-n,-e)
                   | 270 -> (e,-n)
                   | _ -> failwith "bad value"
    
    match xs with
    | [] -> (sn,se)
    | x::xs ->
        match x with
        | 'N',v -> moveWaypoint xs (n+v,e) (sn,se)
        | 'E',v -> moveWaypoint xs (n,e+v) (sn,se)
        | 'S',v -> moveWaypoint xs (n-v,e) (sn,se)
        | 'W',v -> moveWaypoint xs (n,e-v) (sn,se)
        | 'F',v -> moveWaypoint xs (n,e) (sn+n*v,se+e*v)
        | 'L',v -> moveWaypoint xs (rotate -v) (sn,se)
        | 'R',v -> moveWaypoint xs (rotate v) (sn,se)
        | _ -> failwith "bad value"


let solve solver =
    let instructions =
        readLines 12
        |> Seq.map parseInstruction
        |> Seq.toList
    
    let x,y = solver instructions

    string ((abs x)+(abs y))

let solve1 () =
    solve (fun instructions -> moveShip instructions (0,0) 'E')

let solve2 () =
    solve (fun instructions -> moveWaypoint instructions (1,10) (0,0))