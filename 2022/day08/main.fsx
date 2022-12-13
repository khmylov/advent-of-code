open System
open System.IO

type Forest = int[,]

let parse (lines: string[]): Forest =
  Array2D.init (Array.length lines) (lines.[0].Length) (fun i j -> int lines.[i].[j] - int '0')

let input =
  Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
  |> File.ReadAllLines
  |> parse

let pairs y x = seq {for i in 0..y do for j in 0..x do yield i, j}

let maxX = Array2D.length2 input - 1
let maxY = Array2D.length1 input - 1

// 1823
let part1 =
  let visibleIndices =
    Seq.fold
        (fun (visible, maxHeight) (i, height) -> if height > maxHeight then Set.add i visible, height else visible, maxHeight)
        (Set.empty, -1)
    >> fst

  let vertical x =
    let vert' = Seq.map (fun i -> i, input.[i, x]) >> visibleIndices
    Set.union (seq {0..maxX} |> vert') (seq {maxX .. -1..0} |> vert') |> Set.map (fun i -> i, x)

  let horizontal y =
    let hor' = Seq.map (fun j -> j, input.[y, j]) >> visibleIndices
    Set.union (seq {0..maxY} |> hor') (seq {maxY .. -1..0} |> hor') |> Set.map (fun j -> y, j)

  pairs maxY maxX
  |> Seq.fold (fun acc (i, j) -> acc |> Set.union (vertical j) |> Set.union (horizontal i)) Set.empty
  |> Seq.length // 1823

// 211680
let part2 =
  let maxX = maxX - 1
  let maxY = maxY - 1

  pairs maxY maxX
  |> Seq.map (fun (i, j) ->
    let height = input.[i, j]
    [
      seq {for i' in i-1 .. -1 .. 1 -> i', j} // up
      seq {for i' in i+1..maxY -> i', j} // down
      seq {for j' in j-1 .. -1..1 -> i, j'} // left
      seq {for j' in j+1..maxX -> i, j'} // right
    ]
    |> List.map (
        Seq.map (fun (i, j) -> input.[i, j])
        >> Seq.takeWhile ((>) height)
        >> Seq.length
        >> (+) 1
    )
    |> List.fold (*) 1
  )
  |> Seq.max
