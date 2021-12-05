open System
open System.IO
type Polymer = string

let inline diff (a: char) (b: char) = int a - int b
let capitalDiff = diff 'a' 'A' |> abs
let inline areOpposite (a: char) (b: char) = diff a b |> abs = capitalDiff

let zero = char 0

let reduce polymer =
  let res = Array.create (String.length polymer) zero
  let mutable targetIndex = 0
  polymer |> String.iter (fun c -> 
    res.[targetIndex] <- c
    let leftIndex = targetIndex - 1
    if leftIndex >= 0 && areOpposite (res.[leftIndex]) c then do
      res.[leftIndex] <- zero
      res.[targetIndex] <- zero
      targetIndex <- leftIndex
    else do
      targetIndex <- targetIndex + 1
  )
  
  res |> Array.filter ((<>) zero)

let inputFileName = Path.Combine (__SOURCE_DIRECTORY__, "day05-input.txt")
let input = inputFileName |> File.ReadLines |> Seq.head

input
|> reduce
|> Array.length
