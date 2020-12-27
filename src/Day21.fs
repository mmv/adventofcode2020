module Day21

open Utils

let parseLine (line: string) =
    let ingredients, allergens = pairSplit " (contains " line
    
    (
        ingredients.Split(" ") |> set,
        allergens.TrimEnd(')').Split(", ") |> set
    )

let csolve lines =
    let allergens = lines |> Seq.map snd |> Seq.reduce Set.union
    let ingredients = lines |> Seq.map fst |> Seq.reduce Set.union

    let candidates =
        allergens
        |> Seq.map (fun a ->
            lines |> Seq.filter (snd >> Set.contains a) |> Seq.map fst |> Set.intersectMany
        )

    // printfn "Candidates:"
    // for (a,c) in candidates |> Seq.zip allergens do
    //     printfn "%s %A" a c

    let allCandidates = Set.unionMany candidates
    let otherCount =
        lines |> Seq.map fst
        |> Seq.sumBy (fun li -> Set.difference li allCandidates |> Set.count)
    
    let rec solver candidates =
        let singles = candidates |> Seq.filter (Set.count >> ((=) 1)) |> Set.unionMany
        let trimmed = candidates |> Seq.map (fun xs -> if Set.count xs = 1 then xs else Set.difference xs singles)
        if Seq.forall (fun xs -> Set.count xs = 1) trimmed
        then trimmed
        else solver trimmed

    // printfn "Candidates:"
    // for (a,c) in (solver candidates) |> Seq.zip allergens do
    //     printfn "%s %A" a c

    let canonicalList =
        solver candidates
        |> Seq.map (Seq.exactlyOne)
        |> Seq.zip allergens
        |> Seq.sort
        |> Seq.map snd
        |> Seq.reduce (fun a b -> a+","+b)

    otherCount, canonicalList

let solve1 () =
    readLines 21
    |> Array.map parseLine
    |> csolve
    |> fst
    |> string

let solve2 () =
    readLines 21
    |> Array.map parseLine
    |> csolve
    |> snd
    |> string