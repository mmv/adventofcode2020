module Day19

open Utils
open FParsec.Primitives
open FParsec.CharParsers

type Rule<'a> =
    | RuleChoice of 'a seq * 'a seq
    | RuleList of 'a seq
    | RuleChar of char

let parseRule x =
    let ruleId, ruleDef = pairSplit ": " x
    let r =
        match ruleDef with
        | "\"a\"" -> RuleChar 'a' | "\"b\"" -> RuleChar 'b'
        | rs when rs.Contains("|") ->
            let c1,c2 = pairSplit " | " rs
            RuleChoice(
                (c1.Trim().Split(" ") |> Seq.map int),
                (c2.Trim().Split(" ") |> Seq.map int)
            )
        | rs -> RuleList(rs.Trim().Split(" ") |> Seq.map int)

    ruleId, r

// first version using FParsec
let rec foldRules rs idx =
    let foldSeq = Seq.map (foldRules rs)
    match Map.find idx rs with
    | RuleList(xs) -> xs |> foldSeq |> Seq.reduce (>>?)
    | RuleChoice(xs,ys) -> attempt ((attempt (foldSeq xs |> Seq.reduce (>>?))) <|> (attempt (foldSeq ys |> Seq.reduce (>>?))))
    | RuleChar(c) -> pchar c

// FParsec doesn't do well with a lot of backtracking
// so we roll our own parser where each production creates
// a Seq of parsing possibilities which are tried in sequence
// until we're able to match the whole input sequence
let rec runRules rs idx ins =

    let rec foldRuleList xs ins =
        match xs with
        | x::xs ->
            seq {
                for result in runRules rs x ins do
                    match result with
                    | (true, rest) -> yield! foldRuleList xs rest
                    | f -> f
            }
        | [] -> Seq.singleton (true, ins)

    match Map.find idx rs with
    | RuleChar(c) ->
        match ins with
        | x::xs -> Seq.singleton (c = x, xs)
        | _     -> Seq.singleton (false, [])
    | RuleList(xs) -> foldRuleList (xs |> List.ofSeq) ins
    | RuleChoice(xs, ys) ->
        seq {
            yield! foldRuleList (xs |> List.ofSeq) ins
            yield! foldRuleList (ys |> List.ofSeq) ins
        }
        |> Seq.filter fst


let readInput xs =
    let sections = xs |> batchSplit |> Seq.toArray
    let rules =
        sections.[0]
        |> Seq.map (parseRule >> (fun (i,r) -> int i, r))
        |> Map.ofSeq
    let inputs = sections.[1]
    rules, inputs


let solve1 () =
    let rules, inputs = readInput (readLines 18)

    let parser = foldRules rules 0 .>> eof

    inputs
    |> Seq.filter (fun x ->
        match run parser x with
        | Success(_) -> printfn "t %A" x ; true
        | _ -> printfn "f %A" x ; false
    )
    |> Seq.length
    |> string

let solve2 () =
    let rules, inputs = readInput (readLines 18)

    printfn "%A" rules

    let rules' =
        rules
        |> Map.add 8 (RuleChoice([42],[42;8]))
        |> Map.add 11 (RuleChoice([42;31],[42;11;31]))

    let parser = runRules rules' 0

    inputs
    |> Seq.filter (fun x ->
        parser (List.ofSeq x)
        |> Seq.exists (function
                       | (true, []) -> true
                       | _ -> false
        )
    )
    |> Seq.length
    |> string