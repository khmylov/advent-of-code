open System.IO
open System.Text.RegularExpressions

let parseInstruction =
  let regex = Regex("^fold along (.)=(\d+)$", RegexOptions.Compiled)
  fun line ->
    let m = regex.Match line
    if m.Success then Some(m.Groups.[1].Value, m.Groups.[2].Value |> int) else None

let (dots, instructions) =
  Path.Combine (__SOURCE_DIRECTORY__, "input.txt")
  |> File.ReadAllLines
  |> fun lines ->
    lines
    |> Array.takeWhile ((<>) "") 
    |> Array.map (fun s -> let split = s.Split(',') in int split.[1], int split.[0])
    |> Set.ofSeq,
    lines |> Array.choose parseInstruction

let fold input axis axisLine =
  printfn "Folding by %s %d" axis axisLine
  match axis with
  | "y" ->
    let toRemove = input |> Set.filter (fun (i, _) -> i > axisLine)
    toRemove
    |> Seq.fold (fun acc (i, j) -> acc |> Set.add (axisLine - (i - axisLine), j)) (Set.difference input toRemove)
  | "x" ->
    let toRemove = input |> Set.filter (fun (_, j) -> j > axisLine)
    toRemove
    |> Seq.fold (fun acc (i, j) -> acc |> Set.add (i, axisLine - (j - axisLine))) (Set.difference input toRemove)

let solve = Seq.fold (fun acc (axis, line) -> fold acc axis line) dots

let p1 = solve (Array.truncate 1 instructions) |> Set.count // 765
let p2 =
  let result = solve instructions
  let maxX, maxY = result |> Seq.map snd |> Seq.max, result |> Set.map fst |> Seq.max
  let sb = System.Text.StringBuilder()
  for i in 0..maxY do
    for j in 0..maxX do
      sb.Append (if result |> Set.contains (i, j) then '*' else ' ') |> ignore
    sb.AppendLine () |> ignore
  sb.ToString() |> printfn "%s"
