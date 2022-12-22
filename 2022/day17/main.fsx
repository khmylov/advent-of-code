open System

module String =
  let inline trim (s: string) = s.Trim()


let figures = [|
  [
    [1; 1; 1; 1]
  ]
  [
    [0; 1; 0]
    [1; 1; 1]
    [0; 1; 0]
  ]
  [
    [0; 0; 1]
    [0; 0; 1]
    [1; 1; 1]
  ]
  [
    [1]
    [1]
    [1]
    [1]
  ]
  [
    [1; 1]
    [1; 1]
  ]
|]

//let jetPattern = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>" |> Array.ofSeq
let jetPattern = System.IO.Path.Combine(__SOURCE_DIRECTORY__, "input.txt") |> System.IO.File.ReadAllText |> String.trim |> Array.ofSeq

type Chamber = ResizeArray<int[]>

let render figure figX figY chamber =
  Console.Clear()
  for rowIndex, chamberRow in Seq.indexed chamber do
    printf "|"
    for columnIndex, chamberCell in Seq.indexed chamberRow do
      if chamberCell = 1 then printf "#"
      else
        let y, x = rowIndex - figY, columnIndex - figX
        figure
        |> List.tryItem y
        |> Option.bind (List.tryItem x)
        |> function | Some(1) -> "@" | _ -> "."
        |> printf "%s"
    printf "|"
    printfn ""

let rec intersects figure (chamber: Chamber) figX figY =
  match figure with
  | [] -> false
  | _ when figY >= chamber.Count -> true
  | figureRow::restFigure ->
    let chamberRow = chamber[figY]
    let rowMatch =
      figureRow
      |> Seq.mapi (fun i f -> i + figX, f) // translate figure by X
      |> Seq.filter (snd >> (=) 1) // see whether any non-empty cell in this figure row intersects with non-empty cell in chamber
      |> Seq.exists (fun (x, _) -> x < 0 || x >= chamberRow.Length || chamberRow[x] = 1)
    rowMatch || intersects restFigure chamber figX (figY + 1)

let emptyChamber () = seq {for _ in 0..10 -> Array.create 7 0} |> ResizeArray

type NextAction = | Fall | Jet
type State = {Blocks: int64; NextAction: NextAction; FigureIndex: int; JetIndex: int; FigureX: int; FigureY: int}

// Place the given figure into the chamber at specified coords
let append figure figX figY (chamber: Chamber) =
  for rowIndex, row in Seq.indexed figure do
    for column, x in Seq.indexed row do
      if x = 1 then chamber[rowIndex + figY][figX + column] <- 1

// Resize the chamber to accomodate the new figure and find the target row in the chamber for it
let findInitialYAndResize figureIndex (chamber: Chamber) =
  let figureHeight = figures[figureIndex].Length
  let rec loop () =
    chamber
      |> Seq.tryFindIndex (Seq.exists ((=) 1))
      |> Option.defaultValue (chamber.Count)
      |> fun y' -> y' - 3 - figureHeight
      |> fun y -> if y < 0 then chamber.InsertRange(0, emptyChamber()); loop() else y
  loop()

let simulate rounds =
  let chamber = emptyChamber()

  let mutable state = {Blocks = 0L; FigureIndex = 0; FigureX = 2; FigureY = findInitialYAndResize 0 chamber; NextAction = Jet; JetIndex = 0}
  while state.Blocks < rounds do
    let figure = figures[state.FigureIndex]
    // render figure (state.FigureX) (state.FigureY) chamber
    // System.Console.ReadLine() |> ignore

    state <-
      match state.NextAction with
      | Fall ->
        if intersects figure chamber (state.FigureX) (state.FigureY + 1) then
          append figure (state.FigureX) (state.FigureY) chamber
          let nextFigureIndex = (state.FigureIndex + 1) % figures.Length
          {state with
            Blocks = state.Blocks + 1L;
            FigureIndex = nextFigureIndex;
            NextAction = Jet;
            FigureX = 2;
            FigureY = findInitialYAndResize nextFigureIndex chamber}
        else {state with FigureY = state.FigureY + 1; NextAction = Jet}

      | Jet ->
        let nextX = state.FigureX + if jetPattern[state.JetIndex] = '>' then 1 else -1
        {state with
          NextAction = Fall;
          JetIndex = (state.JetIndex + 1) % jetPattern.Length;
          FigureX = if intersects figure chamber nextX (state.FigureY) then state.FigureX else nextX}

  chamber |> Seq.filter (Seq.exists ((=) 1)) |> Seq.length

// 3181
let part1 = simulate 2022

// for part 2 we can memoize (figure_index, jet_index, top_chamber_row_state) -> chamber_length,
// as patterns start to repeat, and the roll it up to the target height

