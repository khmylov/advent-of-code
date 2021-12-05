open System
open System.IO

let times count fn seed = Seq.init count id |> Seq.fold fn seed
let rec build = function
| [] -> 0, []
| childCount::metaCount::tail ->
  if childCount = 0 then
    (0, tail) |> times metaCount (fun (sum, hd::tail) _ -> (sum + hd), tail)
  else
    let (metaRemainder, children) = 
      (tail, Map.empty<int,int>)
      |> times childCount (fun (remainder, children) i ->
        let (childValue, newRemainder) = build remainder
        newRemainder, (Map.add (i+1) childValue children))
    (0, metaRemainder)
    |> times metaCount (fun (sum, (i::tail)) _ ->
      let add = match Map.tryFind i children with Some(childValue) -> childValue | _ -> 0
      (sum + add), tail)

Path.Combine (__SOURCE_DIRECTORY__, "day08-input.txt")
|> File.ReadAllText |> fun s -> s.Split ' ' |> Seq.map (Int32.Parse) |> List.ofSeq
|> build
