open System
open System.IO

type Input = | Ls of Input list | Number of int

type 'a Parser = char list -> ('a * char list) option

module Parser =
  let delay (fn: unit -> 'a Parser): 'a Parser =
    let mutable value = None
    let p arg =
      if value.IsNone then value <- Some(fn())
      value.Value arg
    p

  let inline (|Digit|_|) c = if Char.IsDigit c then Some(int c - int '0') else None

  let inline symbol c = function | hd::tl when hd = c -> Some(c, tl) | _ -> None

  let inline number input =
    let rec loop acc = function
    | Digit(c)::tl -> loop (acc |> Option.defaultValue 0 |> (*) 10 |> (+) c |> Some) tl
    | rest -> acc |> Option.map (fun x -> x, rest)
    loop None input

  // oneOf
  let inline (<|>) p1 p2 arg = p1 arg |> Option.orElseWith (fun () -> p2 arg)
  // useRight
  let inline (>>.) p1 p2 = p1 >> Option.bind (snd >> p2)
  // useLeft
  let inline (.<<) p1 p2 = p1 >> Option.bind (fun (value, r') -> r' |> p2 |> Option.map (fun (_, r'') -> value, r''))

  // map parser result
  let inline (|~>) p1 fn = p1 >> Option.map (fun (res, i') -> fn res, i')

  let inline repeat parser =
    let rec loop acc input =
      match parser input with
      | None -> Some(List.rev acc, input)
      | Some(value, rest) -> loop (value::acc) rest
    loop []

  // then
  let inline (.>>.) (p1: 'a Parser) (p2: 'b Parser) =
    p1 >> Option.bind (fun (a, r') -> r' |> p2 |> Option.map (fun (b, r'') -> (a, b), r''))

  let inline EOF input = if List.isEmpty input then Some((), []) else None

  let empty: unit Parser = fun input -> Some((), input)

  let inline inParens (inner: 'a Parser): 'a Parser = symbol '[' >>. inner .<< symbol ']'

  let rec Lst = delay(fun () ->
    number |~> Number
    <|>
    inParens
      (Lst .>>. repeat (symbol ',' >>. Lst) |~> fun (x, xs) -> Ls(x::xs))
      <|>
      (empty |~> fun _ -> Ls [])
  )

  let Dsl: Input Parser = Lst .<< EOF
  let run input = input |> List.ofSeq |> Dsl |> Option.get |> fst

let parseFrom fileName =
  Path.Combine(__SOURCE_DIRECTORY__, fileName)
  |> File.ReadAllLines
  |> Array.filter (fun s -> s.Length > 0)
  |> Array.map Parser.run

type Result = | Right | NotRight | Unknown

let rec compare left right =
  match left, right with
  | Number l, Number r -> if l < r then Right elif l > r then NotRight else Unknown
  | Ls left, Ls right ->
    let rec loop = function
    | [], [] -> Unknown
    | [], _::_ -> Right
    | _::_, [] -> NotRight
    | l::ls, r::rs -> match compare l r with | Unknown -> loop(ls, rs) | x -> x
    loop(left, right)
  | Number _ as left, (Ls _ as right) -> compare (Ls [left]) right
  | Ls _ as left, (Number _ as right) -> compare left (Ls [right])

let input = parseFrom "input.txt"

// 5808
let part1 =
  input
  |> Seq.chunkBySize 2
  |> Seq.map (fun [|a; b|] -> a, b)
  |> Seq.indexed
  |> Seq.filter (fun (_, (a, b)) -> match compare a b with | Right -> true | _ -> false)
  |> Seq.sumBy (fst >> (+) 1)

// 22713
let part2 =
  let p1, p2 = Ls[Number 2], Ls[Number 6]
  let sorted =
    input
    |> Seq.append [p1; p2]
    |> Seq.sortWith (fun l r -> match compare l r with | Right -> -1 | _ -> 1)
    |> Array.ofSeq
  let i1, i2 = Array.IndexOf(sorted, p1), Array.IndexOf(sorted, p2) in (i1 + 1) * (i2 + 1)
