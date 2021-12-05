open System.IO

let (input, bitLength) =
    Path.Combine (__SOURCE_DIRECTORY__, "input.txt")
    |> File.ReadAllLines
    |> fun lines ->
        lines |> Array.map (fun s -> System.Convert.ToInt32 (s, 2)),
        lines.[0].Length

let inline lastBit number shift = (number >>> shift) % 2
let onesCount numbers shiftBy = numbers |> Seq.filter (fun n -> lastBit n shiftBy = 1) |> Seq.length

// 1307354
let part1 =
    seq {bitLength-1..-1..0}
    |> Seq.map (onesCount input)
    |> Seq.map (fun onesCount -> if onesCount >= input.Length - onesCount then 1 else 0)
    |> Seq.reduce (fun acc bit -> (acc <<< 1) + bit) // binary to decimal
    |> fun gamma ->
        // partial inverse, ignore irrelevant bits
        let epsilon = ~~~(uint gamma) <<< (32 - bitLength) >>> (32 - bitLength)
        in gamma * int epsilon

// 482500
let part2 =
  let rec oxygen bitPosition numbers =
    if Array.length numbers = 1 then numbers.[0]
    else
      let ones = onesCount numbers bitPosition
      let mostCommon = if ones >= numbers.Length - ones then 1 else 0
      numbers
      |> Array.filter (fun n -> lastBit n bitPosition = mostCommon)
      |> oxygen (bitPosition - 1)

  let rec scrubber bitPosition numbers =
    if Array.length numbers = 1 then numbers.[0]
    else
      let ones = onesCount numbers bitPosition
      let leastCommon = if ones >= numbers.Length - ones then 0 else 1
      numbers
      |> Array.filter (fun n -> lastBit n bitPosition = leastCommon)
      |> scrubber (bitPosition - 1)

  oxygen (bitLength - 1) input * scrubber (bitLength - 1) input
