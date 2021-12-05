type Path = {X: int; Y: int; Length: int}
with
  member this.N = {this with Y = this.Y - 1; Length = this.Length + 1}
  member this.S = {this with Y = this.Y + 1; Length = this.Length + 1}
  member this.W = {this with X = this.X - 1; Length = this.Length + 1}
  member this.E = {this with X = this.X + 1; Length = this.Length + 1}

let run (input: string) =
  let chars = input.Substring(1, input.Length - 2) |> List.ofSeq
  let add map path =
    let coord = path.X, path.Y
    match Map.tryFind coord map with
    | Some dist -> Map.add coord (min dist path.Length) map
    | None -> Map.add coord path.Length map

  let consume (map, stack, path: Path) = function
  | 'N' -> add map path.N, stack, path.N
  | 'S' -> add map path.S, stack, path.S
  | 'E' -> add map path.E, stack, path.E
  | 'W' -> add map path.W, stack, path.W
  | '(' -> map, path::stack, path
  | '|' -> map, stack, List.head stack
  | ')' -> let prev::rest = stack in map, rest, prev

  chars
  |> List.fold consume (Map.empty, [], {X=0;Y=0;Length=0})
  |> fun (m, _, _) -> m

open System.IO
let map = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "day20-input.txt")) |> run

// 3699
let part1 = map |> Seq.map (fun x -> x.Value) |> Seq.max
// 8517
let part2 = map |> Seq.filter (fun x -> x.Value >= 1000) |> Seq.length
