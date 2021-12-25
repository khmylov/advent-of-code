open System
open System.IO

let inline parentIndex i = (i - 1) / 2
let inline leftChildIndex i = i * 2 + 1
let inline rightChildIndex i = i * 2 + 2

let SIZE = 63 // 2^(max_depth + 1) - 1, assuming that there is an empty root node at position 0

let parse input =
  let res = Array.create SIZE None
  let rec loop currentIndex = function
  | [] -> ()
  | '['::tl -> loop (leftChildIndex currentIndex) tl
  | ']'::tl -> loop (parentIndex currentIndex) tl
  | ','::tl -> loop currentIndex tl
  | ls ->
    let charList = ls |> List.takeWhile (System.Char.IsDigit)
    let number = charList |> Array.ofList |> String |> Convert.ToInt32
    res.[currentIndex] <- Some number
    loop (currentIndex + 1) (List.skip (charList.Length) ls)
  input
  |> List.ofSeq
  |> loop 0
  res

let indexesAtLevel (depth: int) =
  let count = pown 2 depth
  let startIndex = (pown 2 depth) - 1
  seq {for i in startIndex..(startIndex + count - 1) -> i}

let getDepthOf index = System.Math.Log2 (float (index + 1)) |> int

let isLeafIndex tree index =
  index >= 0 && index < Array.length tree && tree.[index] |> Option.isSome

let explode (tree': _[]) =
  let tree = Array.copy tree'
  let rec findLeftLeaf index =
    let rec loop maxDist candidate currentIndex =
      if currentIndex <= 0 then candidate
      else
        let depth = getDepthOf currentIndex
        let indexes = depth |> indexesAtLevel |> Seq.filter (fun x -> x < currentIndex && isLeafIndex tree x) |> Seq.indexed |> List.ofSeq
        printfn "Found left indexes %A" indexes
        let nextDist, nextCandidate =
          if List.isEmpty indexes then maxDist, candidate
          else
            let d, c = indexes |> List.maxBy fst
            if d > maxDist then d, Some c else maxDist, candidate
        loop nextDist nextCandidate (parentIndex currentIndex)
    loop 0 None (parentIndex index)

  let rec findRightLeaf index =
    let rec loop minDist candidate currentIndex =
      if currentIndex <= 0 then candidate
      else
        let depth = getDepthOf currentIndex
        let indexes = depth |> indexesAtLevel |> Seq.filter (fun x -> x > currentIndex && isLeafIndex tree x) |> Seq.indexed |> List.ofSeq
        printfn "Found right indexes %A" (indexes |> List.map (fun (di, index) -> di, index, tree.[index]))
        let nextDist, nextCandidate =
          if List.isEmpty indexes then minDist, candidate
          else
            let d, c = indexes |> List.minBy fst
            if d < minDist then d, Some c else minDist, candidate
        loop nextDist nextCandidate (parentIndex currentIndex)
    loop (SIZE + 1) None (parentIndex index)

  let pairs = tree |> Seq.indexed |> Seq.skip (SIZE / 2) |> Seq.windowed 2 |> Array.ofSeq
  let pairToExplode =
    pairs
    |> Seq.choose (fun [|ia, a; ib, b|] -> a |> Option.bind (fun a' -> b |> Option.map (fun b' -> (ia, a'), (ib, b'))))
    |> Seq.tryHead
  match pairToExplode with
  | None -> None
  | Some ((i1, a), (i2, b)) as pairToExplode ->
    printfn "Found pair to explode %A" pairToExplode
    match findLeftLeaf i1 with
    | Some leftIndex ->
      let existing = Option.get (tree.[leftIndex])
      printfn "Going to replace left %d with %d at index %d" existing (existing + a) leftIndex
      tree.[leftIndex] <- Some (existing + a)
    | None -> ()

    match findRightLeaf i1 with
    | Some rightIndex ->
      let existing = tree.[rightIndex] |> Option.defaultWith (fun () -> failwithf "Unable to get right value at %d for %d" rightIndex i2)
      printfn "Going to replace right %d with %d at index %d" existing (existing + b) rightIndex
      tree.[rightIndex] <- Some (existing + b)
    | None -> ()

    tree.[i1] <- None
    tree.[i2] <- None
    tree.[parentIndex i1] <- Some 0
    Some tree

let splitNumber value =
  value / 2, (double value) / 2.0 |> System.Math.Ceiling |> int

let split (tree': _[]) =
  let tree = Array.copy tree'
  let splitIndex =
    seq { for depth in 0..5 do for index in indexesAtLevel depth do if isLeafIndex tree index then yield index, Option.get(tree.[index]) }
    |> Seq.tryFind (fun (_, x) -> x > 10)
  match splitIndex with
  | None -> None
  | Some (index, number) ->
    tree.[index] <- None
    let (a', b') = splitNumber number
    tree.[leftChildIndex index] <- Some a'
    tree.[rightChildIndex index] <- Some b'
    Some tree


// let part1 =
//   Path.Combine (__SOURCE_DIRECTORY__, "input.txt")
//   |> File.ReadAllLines
//   |> Array.map parse

let inline indexed inp = inp |> Seq.indexed |> Seq.choose (fun (i, x) -> x |> Option.map (fun x' -> i, x')) |> List.ofSeq

let runPart1 input =
  let rec loop () =
    match explode input with
    | None ->
      match split input with
      | None -> ()
      | Some _ -> loop ()
    | Some _ -> loop ()
  loop ()
  input

// let line1 = "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]" |> parse
// let line2 = line1 |> explode |> Option.get
// let line2Eq = line2 = (parse "[[[[0,7],4],[7,[[8,4],9]]],[1,1]]")


let line3 = parse "[[[[0,7],4],[7,[[8,4],9]]],[1,1]]" |> explode |> Option.get
let line3' = indexed line3
let line3'' = parse "[[[[0,7],4],[15,[0,13]]],[1,1]]" |> indexed
// let line4 = line3 |> split |> Option.get
// let line5 = line4 |> split |> Option.get
// let line6 = line5 |> explode |> Option.get
