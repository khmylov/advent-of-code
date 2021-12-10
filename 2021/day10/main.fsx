open System.IO

let input =
  Path.Combine (__SOURCE_DIRECTORY__, "input.txt")
  |> File.ReadAllLines
  |> Array.map (List.ofSeq)

let pairs = [
  '{', '}'
  '[', ']'
  '(', ')'
  '<', '>'
]

let getOpeningPair c = pairs |> List.tryFind (snd >> (=) c) |> Option.map fst
let getClosingPair c = pairs |> List.tryFind (fst >> (=) c) |> Option.map snd

type Result = Corrupted of char | Open of char list

let validate =
  let rec loop openStack = function
  | [] -> Open openStack
  | nextChar::tl ->
    match getOpeningPair nextChar, openStack with 
    | None, _ -> loop (nextChar::openStack) tl
    | Some current, prev::stack' when current = prev -> loop stack' tl
    | _ -> Corrupted nextChar
  loop []

// 394647
let part1 =
  input
  |> Seq.choose (validate >> function | Corrupted c -> Some c | _ -> None)
  |> Seq.sumBy (function | ')' -> 3 | ']' -> 57 | '}' -> 1197 | '>' -> 25137)

// 2380061249
let part2 =
  input
  |> Seq.choose (validate >> function | Open chars when chars.Length > 0 -> Some chars | _ -> None)
  |> Seq.map (List.choose getClosingPair)
  |> Seq.map (List.fold (fun s c -> s * 5I + match c with | ')' -> 1I | ']' -> 2I | '}' -> 3I | '>' -> 4I) 0I)
  |> Seq.sort
  |> Array.ofSeq
  |> fun arr -> arr.[arr.Length / 2]
