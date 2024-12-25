open System
open System.IO

let input =
  Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
  |> File.ReadAllLines
  |> Array.map (fun line ->
    let [| s1; s2 |] = line.Split(": ")
    let operands = s2.Split(" ") |> Array.map Int64.Parse |> List.ofArray
    Int64.Parse s1, operands)


// 2501605301465
let part1 =
  let rec permute =
    function
    | x1 :: x2 :: xs -> permute ((x1 + x2) :: xs) @ permute ((x1 * x2) :: xs)
    | xs -> xs

  input
  |> Seq.filter (fun (target, operands) -> operands |> permute |> List.exists ((=) target))
  |> Seq.sumBy fst

// 44841372855953
let part2 =
  let inline concatenate a b = a * pown 10L (b |> string |> String.length) + b

  let rec isValid target = function
    | x1::_ when x1 > target -> false
    | x1 :: x2 :: xs ->
      isValid target ((x1 + x2) :: xs) || isValid target ((x1 * x2) :: xs) || isValid target (concatenate x1 x2 :: xs)
    | [x] -> x = target
    | [] -> false

  input
  |> Seq.filter (fun (target, operands) -> isValid target operands)
  |> Seq.sumBy fst
