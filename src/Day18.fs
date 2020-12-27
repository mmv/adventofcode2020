module Day18

open Utils

open FParsec

let exprParser padd pmul =
    let opp = OperatorPrecedenceParser()
    opp.AddOperator(InfixOperator("+", preturn "", padd, Associativity.Left, (+)))
    opp.AddOperator(InfixOperator("*", preturn "", pmul, Associativity.Left, (*)))
    let term = (pint32 |>> bigint) <|> between (pstring "(") (pstring ")") opp.ExpressionParser
    opp.TermParser <- term
    opp.ExpressionParser
    
let solve parser =
    readLines 18
    |> Seq.sumBy (
        fun s ->
            s.Replace(" ", "")
            |> run parser
            |> function
                | Success(a,_,_) -> a
                | _ -> failwith "parse error"
    )
    |> (sprintf "%A")


let solve1 () = solve (exprParser 1 1)

let solve2 () = solve (exprParser 2 1)