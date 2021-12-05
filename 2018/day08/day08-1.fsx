open System
open System.IO

let times count fn seed = Seq.replicate count () |> Seq.fold (fun x _ -> fn x) seed
let rec build = function
| [] -> 0, []
| childCount::metaCount::tail ->
  (0, tail)
  |> times childCount (fun (sum, remainder) ->
    let (sum', remainder') = build remainder
    (sum + sum'), remainder')
  |> times metaCount (fun (sum, hd::tail) -> (sum + hd), tail)

Path.Combine (__SOURCE_DIRECTORY__, "day08-input.txt")
|> File.ReadAllText |> fun s -> s.Split ' ' |> Seq.map (Int32.Parse) |> List.ofSeq
|> build
