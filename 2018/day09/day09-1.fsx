open System.Collections.Generic
type Circle = int * List<int>
type Players = Map<int, int>

type Circle<'T>(capacity: int) =
  let list = new List<'T>(capacity)
  let mutable pointer = -1
  // Positive means "rotate circle buffer clockwise"
  member __.Rotate n =
    if list.Count > 1 then do
      let p' = pointer - n
      pointer <- if p' < 0 then list.Count + p' else p' % list.Count
  member __.Append x =
    pointer <- pointer + 1
    list.Insert(pointer, x)
  member __.Pop () = let value = list.[pointer] in list.RemoveAt(pointer); value
  member __.Count = list.Count

let run (times: int) playersCount =
  let circle = new Circle<int>(times)
  circle.Append 0
  let players = new Dictionary<int,int>()
  for playerNumber in 1..playersCount do
    players.[playerNumber] <- 0

  for marbleValue in 1..times do
    let playerNumber = (marbleValue % players.Count) + 1
    if marbleValue % 23 = 0 then
      circle.Rotate 7
      players.[playerNumber] <- players.[playerNumber] + marbleValue + circle.Pop()
    else
      circle.Rotate -1
      circle.Append marbleValue

  players |> Seq.map (fun x -> x.Value) |> Seq.max

//run 25 9

run 71482 424
