open System.IO
open System.Text.RegularExpressions
open System
open System.Collections.Generic

let inputFileName = (Path.Combine (__SOURCE_DIRECTORY__, "day01-input.txt"))

let numberRegExp = new Regex(@"^([\+-])(\d+)$")

let parse (text: string) =
  let m = numberRegExp.Match text
  if (m.Success && m.Groups.Count = 3) then
    let isPlus = m.Groups.[1].Value = "+"
    let number = Int32.Parse(m.Groups.[2].Value)
    if isPlus then number else -number
  else failwithf "Unable to parse %s" text

let getResult (frequencies: int list) =
  let hashSet = new HashSet<int>()
  let rec step (freqAcc: int) = function
    | [] -> step freqAcc frequencies
    | freq::tail ->
      let nextFreq = freqAcc + freq
      if (hashSet.Add nextFreq) then step nextFreq tail
      else Some(nextFreq)
  step 0 frequencies

let input =
  File.ReadAllLines inputFileName
  |> Seq.filter (fun x -> x.Length > 0)
  |> Seq.map parse
  |> List.ofSeq

let result = getResult input

printfn "Result is %A" result
