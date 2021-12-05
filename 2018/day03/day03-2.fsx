open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic

type Claim = {Id: int; Left: int; Top: int; Width: int; Height: int}

let claimTextRegExp = new Regex(@"^#(\d+) @ (\d+),(\d+): (\d+)x(\d+)$", RegexOptions.Compiled)

let parse (s: string): Claim =
  let m = claimTextRegExp.Match s
  if (m.Success && m.Groups.Count = 6) then
    let inline parseInt (index: int) = m.Groups.[index].Value |> Int32.Parse
    {Id = parseInt 1; Left = parseInt 2; Top = parseInt 3; Width = parseInt 4; Height = parseInt 5 }
  else failwithf "Unable to parse %s" s

let run (claims: Claim seq) =
  let totalWidth = claims |> Seq.map (fun x -> x.Left + x.Width) |> Seq.max
  let totalHeight = claims |> Seq.map (fun x -> x.Top + x.Height) |> Seq.max
  let field: int list [,] = Array2D.create totalHeight totalWidth []
  
  for claim in claims do
    for i in claim.Left..(claim.Left + claim.Width - 1) do
      for j in claim.Top..(claim.Top + claim.Height - 1) do
        let newList = claim.Id :: field.[j, i]
        field.[j, i] <- newList
  
  let overlappingIds = HashSet<int>()
  field |> Array2D.iter (fun ls -> if List.length ls > 1 then ls |> List.iter (overlappingIds.Add >> ignore))
  let nonOverlappingIds = HashSet<int>(claims |> Seq.map (fun x -> x.Id))
  nonOverlappingIds.ExceptWith overlappingIds
  nonOverlappingIds |> Array.ofSeq

let inputFileName = Path.Combine (__SOURCE_DIRECTORY__, "day03-input.txt")
let input = File.ReadAllLines inputFileName
// let input = ["#1 @ 1,3: 4x4"; "#2 @ 3,1: 4x4"; "#3 @ 5,5: 2x2"]

input
|> Seq.map parse
|> run
