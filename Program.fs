open System

type Expr =
    | Number of int
    | Add of Expr * Expr
    | Multiply of Expr * Expr

type Token =
    | NUMBER of int
    | PLUS
    | STAR
    | LPAREN
    | RPAREN
    | EOF

let tokenize (input: string) =
    let rec aux i =
        if i >= input.Length then [EOF]
        else
            match input.[i] with
            | ' ' -> aux (i + 1)
            | '+' -> PLUS :: aux (i + 1)
            | '*' -> STAR :: aux (i + 1)
            | '(' -> LPAREN :: aux (i + 1)
            | ')' -> RPAREN :: aux (i + 1)
            | d when Char.IsDigit d ->
                let j = i + 1
                let rec findEnd k =
                    if k < input.Length && Char.IsDigit input.[k] then findEnd (k + 1)
                    else k
                let endIdx = findEnd j
                let num = Int32.Parse(input.Substring(i, endIdx - i))
                NUMBER(num) :: aux endIdx
            | _ -> failwithf "Unexpected character: %c" input.[i]
    aux 0

let parse (tokens: Token list) =
    let rec parseExpr tokens =
        let rec parseTerm tokens =
            let rec parseFactor tokens =
                match tokens with
                | NUMBER n :: rest -> (Number n, rest)
                | LPAREN :: rest ->
                    let (expr, rest') = parseExpr rest
                    match rest' with
                    | RPAREN :: rest'' -> (expr, rest'')
                    | _ -> failwith "Expected closing parenthesis"
                | _ -> failwith "Unexpected token in factor"

            let rec aux lhs tokens =
                match tokens with
                | STAR :: rest ->
                    let (rhs, rest') = parseFactor rest
                    aux (Multiply(lhs, rhs)) rest'
                | _ -> (lhs, tokens)

            let (lhs, rest) = parseFactor tokens
            aux lhs rest

        let rec aux lhs tokens =
            match tokens with
            | PLUS :: rest ->
                let (rhs, rest') = parseTerm rest
                aux (Add(lhs, rhs)) rest'
            | _ -> (lhs, tokens)

        let (lhs, rest) = parseTerm tokens
        aux lhs rest

    let (ast, tokens') = parseExpr tokens
    match tokens' with
    | [EOF] -> ast
    | _ -> failwith "Unexpected tokens at the end"

let parseInput input =
    let tokens = tokenize input
    parse tokens

let rec printAst (expr: Expr) (indent: string) (isLast: bool) =
    let newIndent = if isLast then indent + "    " else indent + "|   "
    match expr with
    | Number n ->
        printfn "%s%s- Number(%d)" indent (if isLast then "\\" else "+") n
    | Add (left, right) ->
        printfn "%s%s- Add" indent (if isLast then "\\" else "+")
        printAst left newIndent false
        printAst right newIndent true
    | Multiply (left, right) ->
        printfn "%s%s- Multiply" indent (if isLast then "\\" else "+")
        printAst left newIndent false
        printAst right newIndent true

[<EntryPoint>]
let main argv =
    let input = "3 + 5 * (10 + 2)"
    let ast = parseInput input
    printfn "Parsed AST:"
    printAst ast "" true
    0
