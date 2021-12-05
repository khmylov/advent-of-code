// Planets' movemenet along each axis is independent of other axes
// Thus, we can calculate the cycle length for each axis,
// and then find the lowest common multiple of those lengths.
type Dimension = {Pos: int; Vel: int}

let input = [[-1; 7; 3]; [12; 2; -13]; [14; 18; -8]; [17; 4; -4]]
//let input = [[-1; 0; 2]; [2; -10; -7]; [4; -8; 8]; [3; 5; -1]]
let moons = [for i in 0..2 -> input |> List.map (fun ls -> {Pos = ls.[i]; Vel = 0})]

// Apply gravity of other planets to this planet's position and velocity,
// but only for a single axis.
let apply d =
  Seq.sumBy (fun d2 -> sign (d2.Pos - d.Pos))
  >> fun delta ->
    let newVelocity = d.Vel + delta
    {Pos = d.Pos + newVelocity; Vel = newVelocity}

let findCycleLength =
  let rec loop seen step dims =
    let nextDimensions = dims |> List.map (fun d1 -> apply d1 (List.except [d1] dims))
    if Set.contains nextDimensions seen then step
    else loop (Set.add nextDimensions seen) (step + 1L) nextDimensions
  loop (Set.empty) 0L

let rec gcd a b =
  if a = b then a
  elif b = 0L then a
  elif a < b then gcd b a
  else gcd (a - b) b

let lcm a b = a * b / gcd a b
let lcm3 a b c = lcm (lcm a b) c

lcm3 (findCycleLength moons.[0]) (findCycleLength moons.[1]) (findCycleLength moons.[2])
// 402951477454512
