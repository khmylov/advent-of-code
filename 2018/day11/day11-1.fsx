let hundredth number = if number < 100 then 0 else (number % 1000) / 100

let powerLevel serialNumber x y =
  let rackId = x + 10
  hundredth((rackId * y + serialNumber) * rackId) - 5

let sum3x3 grid x y =
  seq { for j in -1..1 do for i in -1..1 do yield x+i, y+j}
  |> Seq.sumBy (fun (x, y) -> Array2D.get grid x y)

let buildGrid serialNumber =
  Array2D.init 300 300 (fun i j -> powerLevel serialNumber (i + 1) (j + 1))

let run grid =
  let maxX = Array2D.length1 grid - 3
  let maxY = Array2D.length2 grid - 3
  let rec aux maxValue maxPoint x y =
    if x > maxX then aux maxValue maxPoint 1 (y + 1)
    else if y > maxY then maxValue, maxPoint
    else
      let sum = sum3x3 grid x y
      if sum > maxValue then aux sum (x, y) (x + 1) y
      else aux maxValue maxPoint (x + 1) y
  aux 0 (1, 1) 1 1

run <| buildGrid 7803
