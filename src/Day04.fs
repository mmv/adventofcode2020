module Day04

open Utils
open FParsec.CharParsers
open FParsec.Primitives

let fields = [|
    
    "byr" // (Birth Year)
    "iyr" // (Issue Year)
    "eyr" // (Expiration Year)
    "hgt" // (Height)
    "hcl" // (Hair Color)
    "ecl" // (Eye Color)
    "pid" // (Passport ID)
    "cid" // (Country ID)

|]

let optionalFields = [| "cid" |]

let allFields (batch: string seq) = batch |> Seq.collect (fun line -> line.Split(" "))

let fieldNames (fields: string seq) =
    fields
    |> Seq.map (pairSplit ":" >> fun (k,_) -> k)

let solve1 () =

    let requiredFields = set(fields) - set(optionalFields)
    let isValid fs = Set.isEmpty (requiredFields - fs)

    readLines 4
    |> batchSplit
    |> Seq.map (allFields >> fieldNames >> set)
    |> Seq.filter isValid
    |> Seq.length
    |> string

let solve2 () =

    // a test that always passes
    let pass _ = true

    /// take a parser, test and input and ensure that the parser
    /// works and the test passes over the parser output
    let valueTest p test str =
        match run (p .>> eof) str with
        | Success(result, _, _) -> test result
        | _ -> false

    let rec prepeat p n =
        if n = 1 then p
        else p .>> (prepeat p (n-1))

    /// Checks a field*value tuple against the validation rules for part 2
    // 
    // byr (Birth Year) - four digits; at least 1920 and at most 2002.
    // iyr (Issue Year) - four digits; at least 2010 and at most 2020.
    // eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
    // hgt (Height) - a number followed by either cm or in:
    //     If cm, the number must be at least 150 and at most 193.
    //     If in, the number must be at least 59 and at most 76.
    // hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
    // ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
    // pid (Passport ID) - a nine-digit number, including leading zeroes.
    // cid (Country ID) - ignored, missing or not.
    // 
    let fieldValidator (k: string, v: string) =
        match (k,v) with
        | "byr", v -> valueTest (pint32) (isBetween 1920 2002) v
        | "iyr", v -> valueTest (pint32) (isBetween 2010 2020) v
        | "eyr", v -> valueTest (pint32) (isBetween 2020 2030) v
        | "hgt", v -> valueTest (pfloat .>>. (pstring "in" <|> pstring "cm")) 
                                (fun (x,u) -> match (x,u) with
                                              | x,"cm" -> isBetween 150. 193. x
                                              | x,"in" -> isBetween  59.  76. x
                                              | _ -> false
                                ) v
        | "hcl", v -> valueTest (pstring "#" >>. (prepeat hex 6)) pass v
        | "ecl", v -> [|"amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"|] |> Array.contains v
        | "pid", v -> valueTest (prepeat digit 9) pass v
        | "cid", _ -> true
        | _ -> false

    let requiredFields = set(fields) - set(optionalFields)
    let isValid fs = Set.isEmpty (requiredFields - fs)
    let both f1 f2 input = (f1 input) && (f2 input)

    readLines 4
    |> batchSplit
    |> Seq.map (allFields
                >> both (fieldNames >> set >> isValid)
                        (Seq.forall (pairSplit ":" >> fieldValidator)))
    |> Seq.filter id
    |> Seq.length
    |> string
