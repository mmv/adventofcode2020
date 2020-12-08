module Day08

open Utils
open FParsec.CharParsers
open FParsec.Primitives
open System.Collections.Immutable


type InstructionSet =
    | NOP
    | ACC
    | JMP

let parseInstruction s =
    pairSplit " " s
    |> fun (instruction, arg) ->
        let instruction' =
            match instruction with
            | "nop" -> NOP
            | "acc" -> ACC
            | "jmp" -> JMP
            | _ -> failwith "unsupported"
        let arg' = int arg
        
        (instruction', arg')

type Instruction = InstructionSet * int

type DebuggerResult<'a,'b> =
    | Continue of 'a
    | Break of 'b

let rec runInstructions (xs: Instruction[]) ip state breaker breakerState =
    match breaker xs ip state breakerState with
    | Break(debugInfo) -> debugInfo
    | Continue(breakerState) ->
        match xs.[ip] with
        | (NOP,_) -> runInstructions xs (ip + 1) state breaker breakerState
        | (ACC,x) -> runInstructions xs (ip + 1) (state + x) breaker breakerState
        | (JMP,x) -> runInstructions xs (ip + x) state breaker breakerState

let breakBeforeRepeat xs ip state bstate =
    if Set.contains ip bstate
    then Break (state)
    else Continue (Set.add ip bstate)

// break with None if we get into a loop
// or break with accumulator state if we reach final instruction
let loopCheck (xs: Instruction[]) ip state bstate =
    if Set.contains ip bstate
    then Break (None)
    else
        if ip = xs.Length
        then Break (Some(state))
        else Continue (Set.add ip bstate)
    
let arraySet xs i x =
    let xs' = Array.copy xs
    Array.set xs' i x
    xs'
    
let solve1 () =
    let instructions =
        readLines 8
        |> Seq.map parseInstruction
        |> Seq.toArray
    
    runInstructions instructions 0 0 breakBeforeRepeat Set.empty
    |> string



let solve2 () =
    let instructions =
        readLines 8
        |> Seq.map parseInstruction
        |> Seq.toArray
    
    instructions
    |> Seq.mapi (fun i x -> i,x)
    |> Seq.filter (fun (_, (x,_)) -> x = JMP)
    |> Seq.map (fun (i,_) ->
        runInstructions (arraySet instructions i (NOP,0)) 0 0 loopCheck (Set.empty)
    )
    |> Seq.filter Option.isSome
    |> Seq.head
    |> Option.get
    |> string
