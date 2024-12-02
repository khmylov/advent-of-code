open System
open System.IO

let input =
  Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
  |> File.ReadAllLines
  |> Array.map (fun x -> x.Split(' ') |> Seq.map Int32.Parse |> List.ofSeq)

let allIncreasing = Seq.forall (fun (x1, x2) -> x2 > x1)
let allDecreasing = Seq.forall (fun (x1, x2) -> x1 > x2)

let diffInRange =
  Seq.forall (fun (x1, x2) -> let diff = abs (x1 - x2) in diff >= 1 && diff <= 3)

let isValid pairs =
  (allIncreasing pairs || allDecreasing pairs) && diffInRange pairs

// 524
let part1 = input |> Seq.map List.pairwise |> Seq.filter isValid |> Seq.length

let removeOne =
  let rec loop head acc =
    function
    | [] -> acc
    | x :: xs -> loop (head @ [ x ]) ((head @ xs) :: acc) xs

  loop [] []

// 569
let part2 =
  input
  |> Seq.filter (fun levels ->
    levels |> List.pairwise |> isValid
    || (levels |> removeOne |> List.map List.pairwise |> Seq.exists isValid))
  |> Seq.length
