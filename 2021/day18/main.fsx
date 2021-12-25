open System

type Element = Number of int | Pair of Element * Element

type 'a Parser = char list -> ('a * char list) option

let mapP (fn: 'a -> 'b) (parser: 'a Parser): 'b Parser = fun input -> input |> parser |> Option.map (fun (res, tail) -> fn res, tail)

let forwardRef () =
  let dummy: 'a Parser = fun _ -> failwith "Unitialized parser ref"
  let parserRef = ref dummy
  let innerFn: 'a Parser = fun input -> !parserRef input
  innerFn, parserRef

let parseNumber: int Parser =
  fun input ->
    match input |> List.tryFindIndex (Char.IsDigit >> not) with
    | None -> let n = Convert.ToInt32 (input |> Array.ofSeq |> String) in Some (n, [])
    | Some 0 -> None
    | Some index -> let n = Convert.ToInt32 (input |> Seq.take index |> Array.ofSeq |> String) in Some (n, List.skip index input)

let parseChar x: Parser<_> = function
| hd::tl when hd = x -> Some(x, tl)
| _ -> None

let (.>>.) (p1: Parser<'a>) (p2: Parser<'b>) =
  p1 >> Option.bind (fun (res1, tail1) -> tail1 |> p2 |> Option.map (fun (res2, tail2) -> (res1, res2), tail2))

let (<|>) (p1: Parser<'a>) (p2: Parser<'a>): Parser<'a> =
  fun input -> input |> p1 |> Option.orElseWith (fun () -> p2 input)

let (>>.) (p1: Parser<'a>) (p2: Parser<'b>): Parser<'b> = p1 >> Option.bind (snd >> p2)
let (.>>) (p1: Parser<'a>) (p2: Parser<'b>): Parser<'a> = p1 >> Option.bind (fun (res, tail) -> tail |> p2 |> Option.map (fun (_, tail2) -> res, tail2))

let parse, parseRef = forwardRef()

let parsePair =
  (parseChar '[' >>. parse .>> parseChar ',') .>>. (parse .>> parseChar ']')
  |> mapP Pair
parseRef.Value <- (parseNumber |> mapP Number) <|> parsePair

// let parseSequence

let getPaths =
  let rec loop current = function
  | Number _ as x -> [x::current |> List.rev]
  | Pair (a, b) as x -> (loop (x::current) a) @ (loop (x::current) b)
  loop []

let toString input =
  let sb = Text.StringBuilder()
  let rec loop = function
  | Number x -> sb.Append x |> ignore
  | Pair (a, b) ->
    sb.Append '[' |> ignore
    loop a
    sb.Append ',' |> ignore
    loop b
    sb.Append ']' |> ignore
  loop input
  sb.ToString()

// let reduce element =
//   let paths = getPaths element
//   paths |> List.tryFind (fun )

//"123ab" |> List.ofSeq |> parseNumber
//"[[[[[9,8],1],2],3],4]" |> List.ofSeq |> parse |> Option.get |> fst |> getPaths |> List.map (fun x -> List.map toString x)
