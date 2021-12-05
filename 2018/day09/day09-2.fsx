open System.Collections.Generic

type Node<'T> = {Value: 'T; mutable Next: Node<'T>; mutable Prev: Node<'T>}
module Node =
  let single value =
    let res = {Value = value; Next = Operators.Unchecked.defaultof<_>; Prev = Operators.Unchecked.defaultof<_>}
    res.Next <- res
    res.Prev <- res
    res
  
type Circle<'T>(initialCursor: Node<'T>) =
  let mutable node = initialCursor
  member __.Append value =
    let next = node.Next
    let newNode = {Value = value; Prev = node; Next = next}
    node.Next <- newNode
    next.Prev <- newNode
    node <- newNode
  member __.Pop () =
    let returnValue = node.Value
    let prev = node.Prev
    let next = node.Next
    prev.Next <- next
    next.Prev <- prev
    node <- next
    returnValue
  member __.Rotate times =
    if times > 0 then for _ in 1..times do node <- node.Prev
    else for _ in -1..-1..times do node <- node.Next

let run times playersCount =
  let circle = Circle(Node.single 0)
  let players = Dictionary<int,int64>()
  for playerNumber in 1..playersCount do players.[playerNumber] <- 0L

  for marbleValue in 1..times do
    let playerNumber = (marbleValue % playersCount) + 1
    if marbleValue % 23 = 0 then
      circle.Rotate 7
      let removedValue = circle.Pop()
      players.[playerNumber] <- players.[playerNumber] + int64(marbleValue) + int64(removedValue)
    else
      circle.Rotate -1
      circle.Append marbleValue
  players |> Seq.map (fun x -> x.Value) |> Seq.max

run 7148200 424
