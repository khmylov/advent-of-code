open System.IO

type Instruction = | Forward of int | Down of int | Up of int

let parseInstruction (text: string) =
    let split = text.Split(' ')
    split.[1]
    |> System.Int32.Parse
    |> match split.[0] with | "forward" -> Forward | "down" -> Down | _ -> Up

let input =
    Path.Combine (__SOURCE_DIRECTORY__, "input.txt")
    |> File.ReadAllLines
    |> Array.map parseInstruction

// 1728414
let part1 =
    let transform (x, y) = function
    | Forward n -> x + n, y
    | Down n -> x, y + n
    | Up n -> x, y - n

    input |> Seq.fold transform (0, 0) ||> (*)

// 1765720035
let part2 =
    let transform (x, y, aim) = function
    | Forward n -> x + n, y + aim * n, aim
    | Down n -> x, y, aim + n
    | Up n -> x, y, aim - n

    input |> Seq.fold transform (0, 0, 0) |> fun (x, y, _) -> x * y
