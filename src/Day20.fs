module Day20

open Utils

let readTileBorder (xs: string seq) =
    let top = xs |> Seq.head
    let bottom = xs |> Seq.last
    let left = System.String.Concat(xs |> Seq.map (fun x -> x.[0]))
    let right = System.String.Concat(xs |> Seq.map Seq.last)

    (top, right, bottom, left)

let srev (s: string) = System.String.Concat(s |> Seq.rev)

let flips (t,r,b,l) = seq {
    yield (t,r,b,l)
    yield (srev t, l, srev b, r)
    yield (b, srev r, t, srev l)
}
let rots (t,r,b,l) = seq {
    yield (t,r,b,l)
    yield (r,srev b,l,srev t)
    yield (srev b,srev l,srev t,srev r)
    yield (srev l,t,srev r,b)
}

let tileNum t = pairSplit " " t |> (snd >> pairSplit ":") |> fst |> int

let read2dtile (xs: string array) =
    array2D xs

let rotate2d tile =
    Array2D.init (Array2D.length2 tile) (Array2D.length1 tile) (fun r c -> tile.[c,^r])

let rotations2d tile =
    seq { 1..3 } |> Seq.scan (fun s _ -> rotate2d s) tile
    
let flips2d tile = seq {
    yield tile
    yield Array2D.init (Array2D.length1 tile) (Array2D.length2 tile) (fun r c -> tile.[^r,c])
    yield Array2D.init (Array2D.length1 tile) (Array2D.length2 tile) (fun r c -> tile.[r,^c])
    yield Array2D.init (Array2D.length1 tile) (Array2D.length2 tile) (fun r c -> tile.[^r,^c])
}

let edges<'a> (tile: 'a [,]) = seq {
    yield tile.[0,*]
    yield tile.[*,0]
    yield tile.[^0,*]
    yield tile.[*,^0]
}

let canMatch t1 t2 =
    let edges1 = edges t1
    let edges2 =
        let e = edges t2 |> set
        e |> Seq.map Array.rev |> set |> Set.union e
    edges1
    |> Seq.exists (fun e -> Set.contains e edges2)

let getMatch<'a when 'a : equality> (board: Map<int*int,string*'a [,]>) (y,x) (tile: 'a [,])  =
    let t = Map.tryFind (y-1,x) board |> Option.map (fun t -> (snd t).[^0,*] = tile.[0,*])
    let l = Map.tryFind (y,x-1) board |> Option.map (fun t -> (snd t).[*,^0] = tile.[*,0])
    let b = Map.tryFind (y+1,x) board |> Option.map (fun t -> (snd t).[0,*] = tile.[^0,*])
    let r = Map.tryFind (y,x+1) board |> Option.map (fun t -> (snd t).[*,0] = tile.[*,^0])
    [| t; l; b; r |] |> Seq.map (Option.defaultValue true) |> Seq.reduce (&&)
    
let rec tilePlacer board tiles cs =
    match tiles with
    | [] -> board
    | ts ->
        let (py,px), tile' =
            ts
            |> Seq.choose(fun t ->
                let augmented = snd t |> flips2d |> Seq.collect rotations2d
                cs
                |> Seq.choose (fun c ->
                    let candidate =
                        augmented
                        |> Seq.filter (getMatch board c)
                        |> Seq.cache
                    
                    if Seq.isEmpty candidate
                    then None
                    else Some(c, (fst t, Seq.head candidate))
                )
                |> Seq.tryHead
            )
            |> Seq.head
        
        let extraCs =
            seq {
                (py-1,px)
                (py+1,px)
                (py,px-1)
                (py,px+1)
            }
            |> Seq.filter (fun c -> not (Map.containsKey c board))
            |> set

        printfn "placed %s" (fst tile')

        tilePlacer
            (Map.add (py,px) tile' board)
            (ts |> List.filter (fun t -> (fst tile') <> (fst t)))
            (cs |> Set.remove (py,px) |> Set.union extraCs)
        
let array2dToSeq a2d = seq {
    for a in 0..(Array2D.length1 a2d)-1 do
        for b in 0..(Array2D.length2 a2d)-1 do
            yield a2d.[a,b]
}        
    
let print2d printer a =
    for y in 0..(Array2D.length1 a)-1 do
        for x in 0..(Array2D.length2 a)-1 do
            printer a.[x,y]
        printfn ""

let solve1 () =
    let tiles =
        readLines 20
        |> batchSplit
        |> Seq.map (fun b -> (Seq.head b, Seq.tail b |> readTileBorder))

    let matching tile ts =
        let (t,r,b,l) = tile
        let tes = [| t; r; b; l; srev t; srev r; srev b; srev l |] |> set

        ts |> Seq.filter (fun t -> Set.intersect t tes |> Set.isEmpty |> not) |> Seq.length 

    let cornerTiles = seq {
        for (t,d) in tiles do
            let ms = matching d (tiles |> Seq.filter (fst >> (<>) t) |> Seq.map (fun (_,(a,b,c,d)) -> set [| a;b;c;d |]))
            if ms = 2 then
                yield tileNum t
    }

    cornerTiles
    |> Seq.map bigint
    |> Seq.reduce (*)
    |> string

let solve2 () =
    let tiles =
        readLines 20
        |> batchSplit
        |> Seq.map (fun b -> (Seq.head b, Seq.tail b |> Seq.toArray |> read2dtile))

    let board = tilePlacer (Map.empty) (Seq.toList tiles) (set [(0,0)])
    let keys = board |> Map.toSeq |> Seq.map fst
    let xs = keys |> Seq.map snd
    let ys = keys |> Seq.map fst

    
    let xmin = Seq.min xs
    let xmax = Seq.max xs
    let ymin = Seq.min ys
    let ymax = Seq.max ys

    let w = tiles |> Seq.head |> snd |> Array2D.length1 |> swap (-) 2
    let megaboardw = ((xmax-xmin+1)*w)
    let megaboardh = ((ymax-ymin+1)*w)
    let megaboard =
        Array2D.init megaboardh megaboardw
            (fun r c -> (snd board.[r/w+ymin,c/w+xmin]).[r%w+1,c%w+1])

    let seaMonster =
        [|
        "                  # "
        "#    ##    ##    ###"
        " #  #  #  #  #  #   "
        |]
        |> Seq.mapi (fun y r -> Seq.mapi (fun x c -> (y,x),c='#') r)
        |> Seq.concat
        |> Seq.filter snd
        |> Seq.map fst

    // print2d (snd >> print2d (printf "%c")) (Array2D.init (ymax-ymin+1) (xmax-xmin+1) (fun r c -> board.[(r+ymin,c+xmin)]))
    // print2d (fst >> printf "%s") (Array2D.init (ymax-ymin+1) (xmax-xmin+1) (fun r c -> board.[(r+ymin,c+xmin)]))
    // print2d (printf "%c") megaboard
    // printfn "%A" megaboard
    
    let monsterPlaces =
        megaboard |> flips2d |> Seq.collect rotations2d
        |> Seq.map(fun megaboard' ->
            { 0..((Array2D.length1 megaboard') - 4) }
            |> Seq.collect (fun y -> { 0..((Array2D.length2 megaboard') - 21)} |> Seq.map (fun x -> (y,x)))
            |> Seq.filter (fun (y,x) -> Seq.forall (fun (y',x') -> megaboard'.[y+y',x+x']='#') seaMonster)
            |> Seq.cache
        )
        |> Seq.filter (Seq.isEmpty >> not)
        |> Seq.head
        |> Seq.length

    megaboard
    |> array2dToSeq
    |> Seq.sumBy (fun c -> if c = '#' then 1 else 0)
    |> fun c -> c - monsterPlaces * 15
    |> string
