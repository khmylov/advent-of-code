open System
open System.IO

let lst1, lst2 =
  Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
  |> File.ReadAllLines
  |> Seq.fold
    (fun (lst1, lst2) line ->
      let [| a; b |] = line.Split(' ', StringSplitOptions.RemoveEmptyEntries) in a :: lst1, b :: lst2)
    ([], [])
  |> fun (lst1, lst2) -> lst1 |> List.map Int32.Parse |> List.sort, lst2 |> List.map Int32.Parse |> List.sort

// 1590491
let part1 = Seq.zip lst1 lst2 |> Seq.sumBy (fun (a, b) -> (a - b) |> abs)

// Extremely inefficient but whatever for this input
// 22588371
let part2 =
  lst1 |> Seq.sumBy (fun a -> lst2 |> Seq.filter ((=) a) |> Seq.length |> (*) a)
