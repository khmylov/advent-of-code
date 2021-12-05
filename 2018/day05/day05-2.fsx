open System
open System.IO

let inline diff (a: char) (b: char) = int a - int b
let capitalDiff = diff 'a' 'A' |> abs
let inline areOpposite a b = diff a b |> abs = capitalDiff

let transform =
  Seq.fold (fun acc c1 -> match acc with c2::tail when areOpposite c1 c2 -> tail | _ -> c1::acc) []

let inputFileName = Path.Combine (__SOURCE_DIRECTORY__, "day05-input.txt")
let input = inputFileName |> File.ReadLines |> Seq.head
// let input = "dabAcCaCBAcCcaDA"

let remove str letter =
  let letter2 = Char.ToLower letter
  str |> Seq.filter (fun c -> c <> letter && c <> letter2)

['A'..'Z']
|> Seq.map (remove input >> transform >> List.length)
|> Seq.min
