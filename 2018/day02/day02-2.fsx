open System.IO

let inline uncurry f (a, b) = f a b

let areClose (str1: string) (str2: string) =
  Seq.zip str1 str2
  |> Seq.map (fun (c1, c2) -> (int c1 - int c2) |> abs)
  |> Seq.filter ((<>) 0)
  |> Seq.length
  |> (=) 1

let getCommonLetters (str1: string) (str2: string) =
  Seq.zip str1 str2
  |> Seq.filter (uncurry (=))
  |> Seq.map fst
  |> fun cs -> new System.String(Array.ofSeq cs)

let inputFileName = (Path.Combine (__SOURCE_DIRECTORY__, "day02-input.txt"))
let input = File.ReadAllLines inputFileName |> Seq.filter (fun x -> x.Length > 0)

seq { for x in input do for y in input do if areClose x y then yield x, y}
|> Seq.head
|> uncurry getCommonLetters
