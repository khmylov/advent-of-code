#time "on"

open System.Collections.Generic

let inline slice (list: List<'a>) startIndex length =
  seq {for index in startIndex..(min(startIndex + length - 1) (list.Count - 1)) -> list.[index]}
  
let inline sequenceEqual xs ys = System.Linq.Enumerable.SequenceEqual(xs, ys)

let recipes = new List<int>([3; 7])
let mutable elf1 = 0
let mutable elf2 = 1

let input =  [8;8;0;7;5;1] 
//let input =  [0;1;2;4;5] 
//let input =  [5;9;4;1;4] 

let mutable foundAt = -1
while foundAt = -1 do
  let val1, val2 = recipes.[elf1], recipes.[elf2]
  let sum = val1 + val2
  let oneDigit = sum < 10
  if oneDigit then recipes.Add sum else recipes.Add 1; recipes.Add (sum - 10) 
  elf1 <- (elf1 + val1 + 1) % recipes.Count
  elf2 <- (elf2 + val2 + 1) % recipes.Count
  
  if recipes.Count > input.Length then do
    for i in 0..(if oneDigit then 0 else 1) do
      let startAt = recipes.Count - input.Length - i
      if sequenceEqual (slice recipes startAt (input.Length)) input then
        foundAt <- startAt
  
  if recipes.Count % 100000 = 0 then
    printfn "Built %d" recipes.Count

printfn "~~~ Built all"

foundAt
