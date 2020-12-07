module Day07

open Utils
open FParsec.CharParsers
open FParsec.Primitives

// input sample:
// mirrored gold bags contain 3 light teal bags.
// clear gold bags contain 5 light maroon bags, 4 pale tomato bags, 5 clear blue bags.
// faded blue bags contain no other bags.

let parseRule s =
    let bag, spec = pairSplit " bags contain " s

    // parser for (quantity: int, bag: string)
    let pspec =
        pint32 .>> pstring " " .>>. (charsTillString " bag" true 250 .>> optional (pstring "s"))
    
    // parser for list of (quantity,bag)
    let pspecs = sepBy pspec (pstring ", ")

    let pFullSpec = pspecs .>> pstring "." .>> eof

    if spec = "no other bags."
    then (bag, List.empty)
    else
        match run pFullSpec spec with
        | Success(r, _, _) -> (bag, r)
        | _ -> failwith $"failed to parse input {s}"

let solve1 () =
    let rules =
        readLines 7
        |> Seq.map parseRule

    let rec traverseRules (typesToFind: string Set) (typesFound: string Set) =
        let newTypes = 
            rules
            |> Seq.filter (snd >> Seq.exists (snd >> typesToFind.Contains))
            |> Seq.map fst
            |> set

        if Set.isEmpty newTypes
        then typesFound
        else traverseRules newTypes (Set.union newTypes typesFound)

    traverseRules (Set.singleton "shiny gold") (Set.empty)
    |> Set.count
    |> string

let solve2 () =
    let rules =
        readLines 7
        |> Seq.map parseRule
        |> Map

    let rec traverseRules bag =
        rules.[bag]
        |> Seq.sumBy (fun (num, bag) ->
                          num + num * (traverseRules bag)
                     )
    
    traverseRules "shiny gold"
    |> string
