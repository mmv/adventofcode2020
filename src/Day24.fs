module Day24

open Utils

let rec pathReduce p (n,e) =
    match p with
    | 'e'::xs      -> pathReduce xs (n,  e+2)
    | 's'::'e'::xs -> pathReduce xs (n-1,e+1)
    | 's'::'w'::xs -> pathReduce xs (n-1,e-1)
    | 'w'::xs      -> pathReduce xs (n,  e-2)
    | 'n'::'w'::xs -> pathReduce xs (n+1,e-1)
    | 'n'::'e'::xs -> pathReduce xs (n+1,e+1)
    | [] -> (n,e)
    | _ -> failwith "invalid pattern"

type Tile = Black | White

let flipTile floor path =
    Map.change
        (pathReduce path (0,0))
        (function
         | Some(Black) -> Some(White)
         | _           -> Some(Black)
        )
        floor

let adj (n,e) =
    let ds = seq {
        (0,2)
        (0,-2)
        (1,1)
        (1,-1)
        (-1,1)
        (-1,-1)
    }
    ds |> Seq.map (fun (a,b) -> (n+a,e+b))

let adjValues p floor =
    adj p
    |> Seq.map (fun a -> Map.tryFind a floor |> Option.defaultValue White)

let flipFloor floor =

    let expand m =
        m
        |> Map.toSeq |> Seq.collect (fst >> adj) |> set
        |> Seq.fold (fun m k ->
                        Map.change k
                                   (function
                                    | None -> Some(White)
                                    | x -> x)
                                   m
                    ) m
    
    let flip k v =
        let adjBlack = adjValues k floor |> Seq.filter ((=) Black) |> Seq.length
        match v with
        | Black -> match adjBlack with
                   | x when x = 0 || x > 2 -> White
                   | _ -> Black
        | White -> match adjBlack with
                   | 2 -> Black
                   | _ -> White
    
    Map.map flip (expand floor)

let solve1 () =
    readLines 24
    |> Seq.map Seq.toList
    |> Seq.fold (flipTile) Map.empty
    |> Map.toSeq
    |> Seq.filter (snd >> ((=) Black))
    |> Seq.length
    |> string

let solve2 () =

    let floor =
        readLines 24
        |> Seq.map Seq.toList
        |> Seq.fold (flipTile) Map.empty

    { 1..100 }
    |> tap (printfn "%A")
    |> Seq.fold (fun f _ -> flipFloor f) floor
    
    |> Map.toSeq
    |> Seq.filter (snd >> ((=) Black))
    |> Seq.length
    |> string