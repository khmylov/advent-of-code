open System.IO
open System.Collections.Generic

let inline uncurry f (a, b) = f a b
type AnalysisResult = { HasTwo: bool; HasThree: bool }

let analyze (id: string): AnalysisResult =
  let map = Dictionary<char, int>()
  id
  |> String.iter (fun c ->
    match map.TryGetValue c with
    | true, num -> map.[c] <- num + 1
    | _ -> map.[c] <- 1)

  let hasTwo = map |> Seq.exists (fun pair -> pair.Value = 2)
  let hasThree = map |> Seq.exists (fun pair -> pair.Value = 3)
  { HasTwo = hasTwo; HasThree = hasThree}

let inputFileName = Path.Combine (__SOURCE_DIRECTORY__, "day02-input.txt")
let input = File.ReadAllLines inputFileName
// let input = [| "abcdef"; "bababc"; "abbcde"; "abcccd"; "aabcdd"; "abcdee"; "ababab" |]

input
|> Seq.map analyze
|> Seq.fold (fun (two, three) x -> 
  (if x.HasTwo then two + 1 else two), (if x.HasThree then three + 1 else three))
  (0, 0)
|> uncurry (*)
