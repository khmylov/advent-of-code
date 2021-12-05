type CellState = | Empty | Vertical | Horizontal | Curve | CurveBack | Intersection
type CartState = | Up | Down | Right | Left
 
type Cart = {Id: int; X: int; Y: int; State: CartState; PassedIntersections: int}
module Cart =
  let tryParse = function
  | '>' -> Some Right | '<' -> Some Left
  | '^' -> Some Up | 'v' -> Some Down
  | _ -> None

  let parseMany (text: string[]) =
    text |> Seq.mapi (fun y line ->
      line |> Seq.mapi (fun x symbol ->
        symbol |> tryParse |> Option.map (fun s -> x, y, s)))
    |> Seq.collect id
    |> Seq.choose id
    |> Seq.fold (fun acc (x, y, s) -> {Id = List.length acc + 1; X = x; Y = y; State = s; PassedIntersections = 0}::acc) []

  let rotate cart =
    let dirs = 
      match cart.State with
      | Up -> [Left; Up; Right] | Down -> [Right; Down; Left]
      | Left -> [Down; Left; Up] | Right -> [Up; Right; Down]
    dirs |> List.item (cart.PassedIntersections % 3)

type Grid = CellState[,]
module Grid =
  let parseChar = function
  | '-' | '>' | '<' -> Horizontal
  | '|' | '^' | 'v' -> Vertical
  | '/' -> Curve | '\\' -> CurveBack
  | '+' -> Intersection
  | _ -> Empty

  let parse (text: string[]) =
    let width = text |> Seq.map (String.length) |> Seq.max
    Array2D.init (text.Length) width (fun y x -> text.[y].[x] |> parseChar)

  let compareLocations c1 c2 =
    if c1.Y < c2.Y then -1
    else if c1.Y > c2.Y then 1
    else if c1.X < c2.X then -1
    else if c1.X > c2.X then 1
    else 0

  let getNext grid cart =
    match cart.State with
    | Up ->
      match Array2D.get grid (cart.Y - 1) cart.X with
      | Vertical -> {cart with Y = cart.Y - 1}
      | Curve -> {cart with Y = cart.Y - 1; State = Right}
      | CurveBack -> {cart with Y = cart.Y - 1; State = Left}
      | Intersection -> {cart with Y = cart.Y - 1; State = Cart.rotate cart; PassedIntersections = cart.PassedIntersections + 1}
      | s -> failwithf "Invalid transition #1: %A %A" cart s
    | Down ->
      match Array2D.get grid (cart.Y + 1) cart.X with
      | Vertical -> {cart with Y = cart.Y + 1; State = Down}
      | Curve -> {cart with Y = cart.Y + 1; State = Left}
      | CurveBack -> {cart with Y = cart.Y + 1; State = Right}
      | Intersection -> {cart with Y = cart.Y + 1; State = Cart.rotate cart; PassedIntersections = cart.PassedIntersections + 1}
      | s -> failwithf "Invalid transition: %A %A" cart s
    | Right ->
      match Array2D.get grid cart.Y (cart.X + 1) with
      | Horizontal -> {cart with X = cart.X + 1; State = Right}
      | Curve -> {cart with X = cart.X + 1; State = Up}
      | CurveBack -> {cart with X = cart.X + 1; State = Down}
      | Intersection -> {cart with X = cart.X + 1; State = Cart.rotate cart; PassedIntersections = cart.PassedIntersections + 1}
      | s -> failwithf "Invalid transition #3: %A %A" cart s
    | Left ->
      match Array2D.get grid cart.Y (cart.X - 1) with
      | Horizontal -> {cart with X = cart.X - 1; State = Left}
      | Curve -> {cart with X = cart.X - 1; State = Down}
      | CurveBack -> {cart with X = cart.X - 1; State = Up}
      | Intersection -> {cart with X = cart.X - 1; State = Cart.rotate cart; PassedIntersections = cart.PassedIntersections + 1}
      | _ -> failwithf "Invalid transision #4"

  let findCrash carts =
    let sorted = carts |> List.sortWith compareLocations
    seq {for c1 in sorted do for c2 in sorted do if c1.Id <> c2.Id then yield c1, c2}
    |> Seq.tryFind (fun (c1, c2) -> c1.Y = c2.Y && c1.X = c2.X)

let run (grid, initialCarts) =
  let rec loop step carts =
    match carts with
    | [] -> None | [x] -> Some x
    | _ ->
      let rec aux completedCarts remainingCarts =
        match Grid.findCrash (completedCarts@remainingCarts) with
        | Some (c1, c2) -> aux (List.except [c1; c2] completedCarts) (List.except [c1; c2] remainingCarts)
        | None ->
          match remainingCarts |> List.sortWith Grid.compareLocations with
          | [] -> loop (step + 1) completedCarts
          | cart::nextRemaining -> aux ((Grid.getNext grid cart)::completedCarts) nextRemaining
      aux [] carts
  loop 0 initialCarts

System.IO.Path.Combine (__SOURCE_DIRECTORY__, "day13-input.txt")
|> System.IO.File.ReadAllLines
|> fun x -> Grid.parse x, Cart.parseMany x
|> run
