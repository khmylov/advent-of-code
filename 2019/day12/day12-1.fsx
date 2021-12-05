let add v1 v2 = List.zip v1 v2 |> List.map (fun (a, b) -> a + b)

type Moon =
    { Pos: int list
      Vel: int list }

let printVec [x; y; z] = sprintf "<x=%d, y=%d, z=%d>" x y z

let printMoon {Pos = pos; Vel = vel} = 
  printfn "pos=%s, vel=%s" (printVec pos) (printVec vel)

let input = [[-1; 7; 3]; [12; 2; -13]; [14; 18; -8]; [17; 4; -4]]
let moons = input |> List.map (fun pos -> {Pos = pos; Vel = [0; 0; 0]})

let gravityDelta v1 v2 =
  List.zip v1 v2 |> List.map (fun (a1, a2) -> sign (a2 - a1))

let apply m moons =
    let velocityDelta =
      List.except [ m ] moons
      |> List.map (fun m2 -> gravityDelta m.Pos m2.Pos)
      |> List.fold add [0; 0; 0]
    let newVelocity = add m.Vel velocityDelta
    {Pos = add m.Pos newVelocity; Vel = newVelocity}

let simulate moons =
    moons |> List.map (fun m1 -> apply m1 moons)

let pot {Pos = pos} = pos |> List.sumBy abs
let kin {Vel = vel} = vel |> List.sumBy abs

let mutable res = moons
for i in 1..1000 do
  printfn "After %d steps:" i
  res <- simulate res
  res |> List.iter printMoon
  let energy = res |> Seq.sumBy (fun m -> let p, k = pot m, kin m in p * k)
  printfn "Total energy: %d" energy
