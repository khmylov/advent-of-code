let hundredth number = (number % 1000) / 100

let powerLevel serialNumber x y =
  let rackId = x + 10
  hundredth((rackId * y + serialNumber) * rackId) - 5

let measure grid = Array2D.length1 grid, Array2D.length2 grid

// https://en.wikipedia.org/wiki/Summed-area_table
let buildSumTable grid =
  let width, height = measure grid
  let table = Array2D.create width height 0
  for j in 0..height-1 do
    for i in 0..width-1 do
      let add = 
        if i > 0 && j > 0 then table.[i - 1, j] + table.[i, j - 1] - table.[i - 1, j - 1]
        else if i > 0 then table.[i - 1, j]
        else if j > 0 then table.[i, j - 1]
        else 0
      table.[i, j] <- grid.[i, j] + add
  table

let sum (sumTable: int[,]) (x, y, size) =
  let i, j = x - 2, y - 2
  sumTable.[i + size, j + size] - sumTable.[i + size, j] - sumTable.[i, j + size] + sumTable.[i, j]

let buildGrid size serialNumber =
  Array2D.init size size (fun i j -> powerLevel serialNumber (i + 1) (j + 1))

let run sumTable =
  let width, height = measure sumTable
  seq {
    for y in 2..height do
      for x in 2..width do
        let upBound = max x y
        for size in 1..height-upBound do yield x, y, size
  }
  |> Seq.maxBy (sum sumTable)

buildGrid 300 7803
|> buildSumTable
|> run

// (230, 272, 17)
