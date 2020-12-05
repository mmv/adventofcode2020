module Day02

open Utils

module F =

    let parseSpec (s: string) =
        let ( range, letter ) = pairSplit " " s
        let ( rmin, rmax ) = pairSplit "-" range |> fun (a,b) -> (int a, int b)
        (rmin, rmax, letter)

    let validator (spec: int * int * string) =
        let min, max, sc = spec
        if sc.Length <> 1
        then failwith "bad length"
        else
            let c = sc.[0]
            fun (s: string) ->
                let counts = s |> Seq.filter ((=) c) |> Seq.length
                min <= counts && counts <= max

    let validator' (spec: int * int * string) =
        let fst, last, letter = spec
        let c = letter.[0]
        fun (s: string) ->
            let a = s.[fst-1]
            let b = s.[last-1]
            (a = c) <> (b = c)

    let splitIns (s: string) =
        let (spec, inp) = pairSplit ":" s
        (spec, inp.Substring(1))

let solve validator =
    readLines 2
    |> Seq.map (fun line ->
                    let spec, input = F.splitIns line
                    (validator <| F.parseSpec spec), input)
    |> Seq.filter (fun (v,x) -> v x)
    |> Seq.length

let solve1 () =    
    solve F.validator    
    |> string

let solve2 () =
    solve F.validator'
    |> string