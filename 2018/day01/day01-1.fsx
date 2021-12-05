open System.IO
open System.Text.RegularExpressions
open System

let inputFileName = (Path.Combine (__SOURCE_DIRECTORY__, "day01-input.txt"))

let numberRegExp = new Regex(@"^([\+-])(\d+)$")

let parse (text: string) =
  let m = numberRegExp.Match text
  if (m.Success && m.Groups.Count = 3) then
    let isPlus = m.Groups.[1].Value = "+"
    let number = Int32.Parse(m.Groups.[2].Value)
    if isPlus then number else -number
  else failwithf "Unable to parse %s" text

let result =
  File.ReadAllLines inputFileName
  |> Seq.filter (fun x -> x.Length > 0)
  |> Seq.map parse
  |> Seq.sum

printfn "Result is %d" result
