open System
open System.Collections.Generic

let getDigits = string >> Seq.map (fun x -> int x - int '0')

type Scoreboard(score1: int, score2: int) =
  let recipes = new List<int>()
  do recipes.Add(score1); recipes.Add(score2)
  let mutable elf1Index = 0
  let mutable elf2Index = 1
  
  member __.Count = recipes.Count
  member __.Score1 = recipes.[elf1Index]
  member __.Score2 = recipes.[elf2Index]
  member __.List = recipes
    
  member this.RunNext() =
    let elf1Offset, elf2Offset = this.Score1 + 1, this.Score2 + 1
    let sum = this.Score1 + this.Score2
    let digits = getDigits sum
    for d in digits do recipes.Add d
    elf1Index <- this.GetNextIndex elf1Index elf1Offset
    elf2Index <- this.GetNextIndex elf2Index elf2Offset
    
  member this.GetNextIndex currentIndex offset =
    (currentIndex + offset) % recipes.Count

let run count =
  let scoreboard = Scoreboard(3, 7)
  while scoreboard.Count < count + 10 do scoreboard.RunNext()
  scoreboard.List
  |> Seq.skip count
  |> Seq.take 10
  |> fun x -> String.Join("", x)

run 880751
