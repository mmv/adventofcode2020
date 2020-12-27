module Day17

open Utils

let getState space coords = Map.tryFind coords space |> Option.defaultValue false

let neighboorDeltas =
    seq {
        for a in  { -1..1 } do
            for b in { -1..1 } do
                for c in { -1..1 } do
                    if (a,b,c) <> (0,0,0)
                    then yield (a,b,c)
    }
    |> Seq.toArray

let neighboorDeltas4 =
    seq {
        for a in  { -1..1 } do
            for b in { -1..1 } do
                for c in { -1..1 } do
                    for d in { -1..1 } do
                        if (a,b,c,d) <> (0,0,0,0)
                        then yield (a,b,c,d)
    }
    |> Seq.toArray

let neighboorCoords (x,y,z) =
    neighboorDeltas |> Seq.map (fun (a,b,c) -> (a+x,b+y,c+z))

let neighboorCoords4 (x,y,z,w) =
    neighboorDeltas4 |> Seq.map (fun (a,b,c,d) -> (a+x,b+y,c+z,d+w))

let neighboorVals space coords =
    neighboorCoords coords
    |> Seq.map (getState space)

let neighboorVals4 space coords =
    neighboorCoords4 coords
    |> Seq.map (getState space)

let iterSpace ((x1,x2),(y1,y2),(z1,z2)) =
    seq {
        for x in x1..x2 do
            for y in y1..y2 do
                for z in z1..z2 do
                    yield (x,y,z)
    }

let iterSpace4 ((x1,x2),(y1,y2),(z1,z2),(w1,w2)) =
    seq {
        for x in x1..x2 do
            for y in y1..y2 do
                for z in z1..z2 do
                    for w in w1..w2 do
                        yield (x,y,z,w)
    }

let expandSpace ((x1,x2),(y1,y2),(z1,z2)) a =
    ((x1-a,x2+a), (y1-a,y2+a), (z1-a,z2+a))

let expandSpace4 ((x1,x2),(y1,y2),(z1,z2),(w1,w2)) a =
    ((x1-a,x2+a), (y1-a,y2+a), (z1-a,z2+a), (w1-a,w2+a))

let sumBool = Seq.sumBy (fun x -> if x then 1 else 0)

let cycle expander neigh iter space spaceConfig =
    let expandedConfig = expander spaceConfig 1
    let space' =
        iter expandedConfig
        |> Seq.map
            (fun k ->
                let v = getState space k
                let nc = sumBool (neigh space k)
                let v' =
                    if v then
                        match nc with
                        | 2 -> true
                        | 3 -> true
                        | _ -> false
                    else nc = 3
                (k,v')
            )
        |> Map.ofSeq
        
    (space', expandedConfig)

let cycle3 = cycle expandSpace neighboorVals iterSpace 
let cycle4 = cycle expandSpace4 neighboorVals4 iterSpace4

let readSpace xss =
    xss
    |> Seq.mapi (fun y xs -> xs |> Seq.mapi (fun x v -> ((x,y,0),v = '#')))
    |> Seq.concat
    |> Map.ofSeq

let readSpace4 xss =
    readSpace xss |> Map.toSeq |> Seq.map (fun ((x,y,z),v) -> ((x,y,z,0),v)) |> Map.ofSeq

let solve1 () =
    let input = readLines 17
    let space = input |> readSpace
    let spaceLimits = (0, Seq.length input), (0, (Seq.length << Seq.head) input), (0,0)
    
    let finalSpace, _ =
        Seq.init 6 ignore
        |> Seq.fold (fun (space,limits) _ -> cycle3 space limits) (space, spaceLimits)

    finalSpace
    |> Map.toSeq
    |> Seq.map snd
    |> sumBool
    |> string

let solve2 () =
    let input = readLines 17
    let space = input |> readSpace4
    let spaceLimits = (0, Seq.length input), (0, (Seq.length << Seq.head) input), (0,0), (0,0)
    
    let finalSpace, _ =
        Seq.init 6 ignore
        |> Seq.fold (fun (space,limits) _ -> cycle4 space limits) (space, spaceLimits)

    finalSpace
    |> Map.toSeq
    |> Seq.map snd
    |> sumBool
    |> string