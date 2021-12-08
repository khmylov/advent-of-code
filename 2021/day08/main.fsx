open System.IO

let inline split (separator: string) (s: string) = s.Split(separator)

let input =
  Path.Combine (__SOURCE_DIRECTORY__, "input.txt")
  |> File.ReadAllLines
  |> Array.map (
    split " | "
    >> Array.map (split " ")
    >> fun xs ->
      xs.[0] |> Array.map (List.ofSeq >> List.sort) |> Array.sortBy (List.length),
      xs.[1])

// 449
let part1 =
  input |> Seq.collect snd |> Seq.filter (fun s -> List.contains s.Length [2; 3; 4; 7]) |> Seq.length

let DIGITS = [|
  ['a'; 'b'; 'c'; 'e'; 'f'; 'g']
  ['c'; 'f'] // digit 1
  ['a'; 'c'; 'd'; 'e'; 'g']
  ['a'; 'c'; 'd'; 'f'; 'g']
  ['b'; 'c'; 'd'; 'f'] // digit 4
  ['a'; 'b'; 'd'; 'f'; 'g']
  ['a'; 'b'; 'd'; 'e'; 'f'; 'g']
  ['a'; 'c'; 'f'] // digit 7
  ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'] // digit 8
  ['a'; 'b'; 'c'; 'd'; 'f'; 'g']
|]

let rec permutations = function
| [] -> seq [[]]
| x::xs -> Seq.collect (insertions x) (permutations xs)
and insertions x = function
| [] -> [[x]]
| y::ys as xs -> (x::xs)::List.map (fun x' -> y::x') (insertions x ys)

let inline translate mapping = List.map (fun x -> mapping |> Array.item (int x - int 'a'))

let isMatch mapping signal digit =
  if List.length signal <> List.length digit then false
  else signal |> translate mapping |> List.sort |> (=) digit

let testMany signals mapping =
  signals |> Seq.forall (fun signal -> DIGITS |> Array.exists (isMatch mapping signal))

let possibleMappings = permutations ['a'..'g'] |> Seq.map (Array.ofSeq) |> List.ofSeq

// 968175
let part2 =
  input
  |> Seq.map (fun (signals, numbers) -> 
    let actualMapping = possibleMappings |> List.find (testMany signals)
    numbers
    |> Seq.map (List.ofSeq >> translate actualMapping >> List.sort)
    |> Seq.map (fun x -> DIGITS |> Array.findIndex ((=) x))
    |> Seq.fold (fun acc digit -> acc * 10 + digit) 0
  )
  |> Seq.sum
