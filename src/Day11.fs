module Day11

open Utils

type Seat = Floor | Empty | Occupied
type FloorPlan = Map<int * int, Seat>

let readFloorInput () =

    let parseLine xs =
        Seq.map (function
                 | '.' -> Floor
                 | 'L' -> Empty
                 | '#' -> Occupied
                 | _ -> failwith "invalid"
        ) xs

    readLines 11
    |> Seq.map parseLine
    |> Seq.mapi (
        fun r row -> Seq.mapi (fun c cell -> ((r,c), cell)) row
    )
    |> Seq.concat
    |> Map

let getSpot (m: FloorPlan) p =
    if m.ContainsKey(p) then m.[p] else Floor

let getAdjSpots (p: int*int) = seq {
    for r in -1..1 do
        for c in -1..1 do
            let x,y = p
            if (c,r) <> (0,0) then yield (r+x,c+y)
}

// apply rules to decide how the seat at `p` will switch
let nextStep (m: FloorPlan) (p: int*int) =
    let counts =
        getAdjSpots p
        |> Seq.countBy (getSpot m)
        |> Map

    match m.[p] with
    | Empty -> if Map.tryFind Occupied counts = None
               then Occupied else Empty
    | Occupied -> if (Map.tryFind Occupied counts |> Option.defaultValue 0) >= 4 
                  then Empty else Occupied
    | x -> x


let countSeenSpots (m: FloorPlan) (p: int*int) =
    let padd (a,b) (x,y) = (a+x,b+y)
    let directions = getAdjSpots (0,0)
    let rec see p direction =
        let p' = padd p direction
        match Map.tryFind p' m with
        | Some(Floor) -> see p' direction
        | Some(x) -> x
        | None -> Floor
    
    directions
    |> Seq.map (see p)
    |> Seq.countBy id

// apply the rules for part 2 on how the seat at `p` will switch
let nextStep2 (m: FloorPlan) (p: int*int) =
    let counts = countSeenSpots m p |> Map
    match m.[p] with
    | Empty -> if Map.tryFind Occupied counts = None
               then Occupied else Empty
    | Occupied -> if (Map.tryFind Occupied counts |> Option.defaultValue 0) >= 5
                  then Empty else Occupied
    | x -> x

let rec iterateFloor iterator (m: FloorPlan) =
    let newPlan = Map.map (fun p _ -> iterator m p) m
    if newPlan = m
    then m
    else iterateFloor iterator newPlan

let solve iterator =
    let floor =
        readFloorInput ()
        |> iterateFloor iterator

    floor |> Seq.countBy (fun kvp -> getSpot floor (kvp.Key))
          |> Map
          |> Map.tryFind Occupied
          |> Option.defaultValue 0
          |> string

let solve1 () = solve nextStep
let solve2 () = solve nextStep2