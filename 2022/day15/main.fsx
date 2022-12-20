open System
open System.IO

type Point = { X: int; Y: int }
module Point =
  let inline distance {X = x1; Y = y1} {X = x2; Y = y2} = abs(x1 - x2) + abs(y1 - y2)

let input =
  let regex = Text.RegularExpressions.Regex("(-?\\d+)")

  Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
  |> File.ReadAllLines
  |> Seq.map (
      regex.Matches
      >> Seq.map (fun x -> x.Value |> Int32.Parse)
      >> List.ofSeq >>
      function [a;b;c;d] -> {X = a; Y = b}, {X = c; Y = d})
  |> List.ofSeq

// Range scanned by scanner at row Y
let inline rangeAt y scanner nearestBeacon =
  let length = Point.distance scanner nearestBeacon - abs(scanner.Y - y)
  if length < 0 then None else Some(scanner.X - length, scanner.X + length)

let inline isWithin x l r = x >= l && x <= r

let inline merge ranges =
  let rec loop acc = function
  | [] -> acc
  | (l1, r1)::(l2, r2)::tl when isWithin l2 l1 r1 && isWithin r2 l1 r1 -> loop acc ((l1, r1)::tl)
  | a::b::tl when snd a >= fst b -> loop acc ((fst a, snd b)::tl)
  | a::tl -> loop (a::acc) tl
  loop [] ranges |> List.rev

// 4886370
let part1 =
  let scanY = 2000000

  input
  |> Seq.choose (fun (s, b) -> rangeAt scanY s b)
  |> Seq.sort
  |> List.ofSeq
  |> merge
  |> Seq.sumBy (fun x -> snd x - fst x)

// 11374534948438
// Can improve further by reducing inner lambda allocations,
// and by faster calculation of per-scanner ranges by keeping a state from the previous row and increasing/decreasing based on (scanner.Y - y)
let part2 =
  seq { 0 .. 4000000 }
  |> Seq.map (fun scanY ->
    input
    |> Seq.choose (fun (s, b) -> rangeAt scanY s b)
    |> Seq.sort
    |> List.ofSeq
    |> merge
    |> fun ls -> scanY, ls
  )
  |> Seq.find (fun (_, ls) -> ls.Length > 1) // expected to have only 1 row with 2 ranges with a gap of 1 x element
  |> fun (y, [(_, x); _]) -> x + 1 |> int64 |> (*) 4000000L |> (+) (int64 y)
