module Day14

open Utils
open FParsec.Primitives
open FParsec.CharParsers

let progMem = Map.empty<int,int64>

let parseMask xs =
    let ands =
        xs |> Seq.map (fun x -> if x = '0' then 0L else 1L) |> Seq.reduce (fun s x -> (s<<<1) + x)
    let ors =
        xs |> Seq.map (fun x -> if x = '1' then 1L else 0L) |> Seq.reduce (fun s x -> (s<<<1) + x)
    let mask36 = ((1L<<<36) - 1L)

    (ands &&& mask36, ors &&& mask36)

type Instructions =
    | SetMask of string
    | SetMem of int64 * int64

let inputParsers =
    (
        pstring "mask = " >>. regex "\\w+"
        |>> SetMask
    ) <|> (
        pstring "mem[" >>. pint64 .>> pstring "] = " .>>. pint64 
        |>> SetMem
    )

let parseInput s =
    match run inputParsers s with
    | Success(r, _, _) -> r
    | Failure(_) -> failwith "bad input"

let setMem mem addr (ands,ors) (v: int64) =
    let v' = (v ||| ors) &&& ands
    Map.add addr v' mem

let runInput (mem,mask) = function
    | SetMem(addr,v) -> ((setMem mem addr mask v), mask)
    | SetMask(m) -> (mem, (parseMask m))

let setMem' mem addr mask (v: int64) =
    let rec getAddrs addr mask =
        // printfn "m %A" addr
        match mask with
        | [] -> [0L]
        | '0'::bs -> (getAddrs (addr>>>1) bs) |> List.map (fun x -> (x <<< 1) ||| (addr &&& 1L))
        | '1'::bs -> (getAddrs (addr>>>1) bs) |> List.map (fun x -> (x <<< 1) ||| 1L)
        | 'X'::bs -> let rest = (getAddrs (addr>>>1) bs)
                     List.append
                        (rest |> List.map (fun x -> (x <<< 1)))
                        (rest |> List.map (fun x -> (x <<< 1) ||| 1L))
        | _ -> failwith "invalid"

    getAddrs addr (mask |> List.rev)
    |> Seq.fold (fun m a -> Map.add a v m) mem
         


let solve1 () =
    
    let mem, _ = 
        readLines 14
        |> Seq.map parseInput
        |> Seq.fold runInput (Map.empty, (0L,0L))

    mem
    |> Map.toSeq
    |> Seq.sumBy snd
    |> string



let solve2 () =

    let runInput (mem,mask) = function
        | SetMem(addr,v) -> ((setMem' mem addr mask v), mask)
        | SetMask(m) -> (mem, m |> Seq.toList)

    let mem, _ =
        readLines 14
        |> Seq.map parseInput
        |> Seq.fold runInput (Map.empty, List.empty)

    mem
    |> Map.toSeq |> Seq.sumBy snd |> string
