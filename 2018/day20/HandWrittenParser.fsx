type public Direction = | N | S | W | E
type SeqExpression = Expression list
and Expression = | Dir of Direction | Branch of SeqExpression list

module String = let get index (str: string) = str.[index]

module HandWrittenParser =
  let parseDirection str pos =
    match String.get pos str with | 'N' -> Some N | 'S' -> Some S | 'E' -> Some E | 'W' -> Some W | _ -> None
    |> Option.map (fun d -> Dir d, pos + 1)

  let rec private parseSeq str pos =
    let rec step acc pos =
      match String.get pos str with
      | '(' ->
        let (expr, nextPos) = parseBranch str (pos + 1)
        step (expr::acc) (nextPos + 1)
      | _ ->
        match parseDirection str pos with 
        | Some (dir, nextPos) -> step (dir::acc) nextPos
        | None -> acc, pos
    let (exprs, nextPos) = step [] pos
    exprs |> List.rev, nextPos

  and private parseBranch str pos =
    let rec step cases pos =
      let (exprSeq, nextPos) = parseSeq str pos
      match str.[nextPos] with
      | ')' -> exprSeq::cases, nextPos
      | '|' when str.[nextPos + 1] = ')' -> []::exprSeq::cases, nextPos + 1
      | '|' -> step (exprSeq::cases) (nextPos + 1)
      | c -> failwithf "parseBranchList: Unexpected char %c at pos %d" c nextPos
    let (cases, nextPos) = step [] pos
    cases |> List.rev |> Branch, nextPos

  let parse str = parseSeq str 1 |> fst

module Printer =
  let print expressions =
    let sb = System.Text.StringBuilder()
    let rec printExpr = function
    | Dir d -> (match d with | N -> 'N' | S -> 'S' | E -> 'E' | W -> 'W') |> sb.Append |> ignore
    | Branch cases ->
      sb.Append '(' |> ignore
      cases
      |> List.iteri (fun i case -> 
        printSeq case
        if i <> List.length cases - 1 then
          sb.Append '|' |> ignore
      )
      sb.Append(')') |> ignore
    and printSeq expressions =
      for e in expressions do
        printExpr e

    sb.Append '^' |> ignore
    printSeq expressions
    sb.Append '$' |> ignore

    sb.ToString()

let assertEq a b =
  if a <> b then failwithf "Expected equality: %A and %A" a b

let assertParse input expected =
  let parsed = HandWrittenParser.parse input
  assertEq parsed expected
  let printed = Printer.print expected
  assertEq printed input

assertParse "^WNE$" [Dir W; Dir N; Dir E]
assertParse
  "^ENWWW(NEEE|SSE(EE|N))$"
  [
    Dir E; Dir N; Dir W; Dir W; Dir W;
    Branch [
      [Dir N; Dir E; Dir E; Dir E];
      [
        Dir S;
        Dir S;
        Dir E;
        Branch [
          [Dir E; Dir E];
          [Dir N]
        ]
      ]
    ]
  ]
  
assertParse
  "^(NEWS|WNSE|)$"
  [
    Branch [
      [Dir N; Dir E; Dir W; Dir S];
      [Dir W; Dir N; Dir S; Dir E];
      []
    ]
  ]

assertParse
  "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$"
  [
    Dir E; Dir N; Dir N; Dir W; Dir S; Dir W; Dir W;
    Branch [
      [Dir N; Dir E; Dir W; Dir S];
      []
    ];
    Dir S; Dir S; Dir S; Dir E; Dir E; Dir N;
    Branch [
      [Dir W; Dir N; Dir S; Dir E];
      []
    ];
    Dir E; Dir E;
    Branch [
      [Dir S; Dir W; Dir E; Dir N];
      []
    ];
    Dir N; Dir N; Dir N
  ]
