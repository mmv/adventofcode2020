module Day16

open Utils

type IntRange = IntRange of lower: int * upper: int

let checkRange r x =
    match r with
    | IntRange(lower,upper) when lower <= x && x <= upper -> true
    | _ -> false

type Rule = Rule of name: string * IntRange seq

let readRule x =
    let rName, rSpec = pairSplit ": " x
    let rs = rSpec.Split(" or ")
             |> Array.map (pairSplit "-" >> pairMap int >> IntRange)
    Rule(rName, rs)

let readInput xs =
    let sections = batchSplit xs |> Seq.toArray
    let rules = sections.[0] |> Seq.map readRule |> Seq.cache
    let myTicket = (sections.[1] |> Seq.last).Split(",") |> Array.map int
    let otherTickers =
        sections.[2]
        |> Seq.tail
        |> Seq.map (fun t -> t.Split(",") |> Array.map int)
        |> Seq.cache
    
    (rules, myTicket, otherTickers)

let scanErrors rules tickets =
    let allRanges = Seq.collect (fun (Rule(_,rs)) -> rs) rules
    tickets
    |> Seq.collect (
        Seq.filter (
            fun n -> not <| Seq.exists (fun r -> checkRange r n) allRanges
        ))

let discardInvalid rules tickets =
    let allRanges = Seq.collect (fun (Rule(_,rs)) -> rs) rules
    tickets
    |> Seq.filter (
        Seq.forall (
            fun n -> Seq.exists (fun r -> checkRange r n) allRanges
        ))

let ruleFinder rules tickets =
    let values = Seq.transpose tickets |> Seq.cache

    let ruleMatchesValues (Rule(_,rs)) vs =
        vs
        |> Seq.forall (fun v -> Seq.exists (fun r -> checkRange r v) rs)

    let cols = values |> Seq.indexed |> Map.ofSeq

    let rec partialFind rules cols =
        printfn "pf -> %A %A" rules cols
        if Map.isEmpty cols
        then Map.empty
        else
            let matches =
                cols
                |> Map.toSeq
                |> Seq.choose
                    (fun (i,vs) ->
                        Seq.filter
                            (fun rule -> ruleMatchesValues rule vs)
                            rules
                        |> Seq.tryExactlyOne
                        |> Option.map (fun r -> (i,r))
                    )
                |> Map.ofSeq

            let others =
                partialFind
                    (rules |> Seq.except (matches |> Map.toSeq |> Seq.map snd))
                    (cols |> Map.filter (fun k _ -> not <| matches.ContainsKey k))

            Map.fold (fun m k v -> Map.add k v m) matches others

    partialFind rules cols
                    
let solve1 () =
    let rules, _, tickets =
        readLines 16
        |> readInput

    scanErrors rules tickets
    |> Seq.reduce (+)
    |> string

let solve2 () =
    let rules, myTicket, tickets =
        readLines 16
        |> readInput

    let validTickets =
        discardInvalid rules tickets
        // |> Seq.append (Seq.singleton myTicket)
    
    ruleFinder rules validTickets
    |> Map.toSeq
    |> Seq.filter (fun (_, Rule(n,_)) -> n.StartsWith("departure"))
    |> Seq.map (fst >> Array.get myTicket >> bigint)
    |> Seq.reduce (*)
    |> string