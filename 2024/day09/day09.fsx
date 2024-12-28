open System
open System.IO
open System.Text

let input =
  Path.Combine(__SOURCE_DIRECTORY__, "input.txt") |> File.ReadAllText |> _.Trim()

let inline swap (lst: ResizeArray<_>) i j =
  let tmp = lst[j]
  lst[j] <- lst[i]
  lst[i] <- tmp

// 6395800119709
let part1 =
  let state = ResizeArray<int>()

  let appends =
    Seq.scan
      (fun (_, isFile, fileId) c ->
        let append = Seq.replicate (int <| c - '0') (if isFile then fileId else -1)
        (append, not isFile, if isFile then fileId + 1 else fileId))
      ([], true, 0)
      input

  for append, _, _ in appends do
    state.AddRange(append)

  let findNextGap idx =
    seq { idx + 1 .. state.Count - 1 }
    |> Seq.tryFind (fun j -> state[j] = -1)
    |> Option.defaultValue Int32.MaxValue

  let findNextFile idx =
    seq { idx - 1 .. -1 .. 0 }
    |> Seq.tryFind (fun j -> state[j] <> - 1)
    |> Option.defaultValue -1

  let rec loop leftGapIdx rightFileIdx =
    if leftGapIdx <= rightFileIdx then
      swap state leftGapIdx rightFileIdx

      loop (findNextGap leftGapIdx) (findNextFile rightFileIdx)

  loop (findNextGap -1) (findNextFile state.Count)

  state
  |> Seq.map int64
  |> Seq.indexed
  |> Seq.takeWhile (fun (_, x) -> x <> -1)
  |> Seq.sumBy (fun (idx, fileIdx) -> int64 idx * fileIdx)

type Content =
  | File of Index: int * Length: int
  | Free of int

// 6418529470362
let part2 =
  let state =
    input
    |> Seq.scan
      (fun (_, isFile, fileId) c ->
        let idx = int <| c - '0'
        let content = if isFile then File(fileId, idx) else Free(idx)
        content, not isFile, if isFile then fileId + 1 else fileId)
      (Free(0), true, 0)
    |> Seq.skip 1
    |> Seq.map (fun (x, _, _) -> x)
    |> ResizeArray

  for rightIdx in state.Count - 1 .. -1 .. 0 do
    match state[rightIdx] with
    | Free _ -> ()
    | File(_, fileLength) ->
      Seq.indexed state
      |> Seq.tryPick (fun (idx, x) ->
        match x with
        | Free(freeSpace) when freeSpace >= fileLength -> Some(idx, freeSpace)
        | _ -> None)
      |> Option.iter (fun (leftGapIdx, freeSpace) ->
        if leftGapIdx <= rightIdx then
          swap state leftGapIdx rightIdx

          if freeSpace <> fileLength then
            state.Insert(leftGapIdx + 1, Free(freeSpace - fileLength))
            state[rightIdx + 1] <- Free(fileLength))

  let mutable blockIdx = 0
  let mutable result = 0L
  for item in state do
    match item with
    | Free(space) -> blockIdx <- blockIdx + space
    | File(fileIdx, fileLength) ->
      for i in 1..fileLength do
        result <- result + int64 blockIdx * int64 fileIdx
        blockIdx <- blockIdx + 1
  result
