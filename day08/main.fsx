open System.IO

type Instruction = | Nop of int | Acc of int | Jmp of int

let parseNumber (str: string) = if str.StartsWith "-" then -(str.[1..] |> int) else (str.[1..] |> int)

let parseInstruction (str: string) =
    let kind =
        match str.[0..2] with
        | "nop" -> Nop
        | "acc" -> Acc
        | "jmp" -> Jmp
    kind (parseNumber str.[3..])

type EvalResult = | Loop of int | NoLoop of int

let eval instructions =
    let rec loop seen accumulator currentIndex =
        if currentIndex >= Array.length instructions then NoLoop(accumulator)
        elif Set.contains currentIndex seen then Loop(accumulator)
        else
            let nextAcc, nextIndex = 
                match instructions.[currentIndex] with
                | Nop _ -> accumulator, currentIndex + 1
                | Acc x -> accumulator + x, currentIndex + 1
                | Jmp x -> accumulator, currentIndex + x

            loop (Set.add currentIndex seen) nextAcc nextIndex

    loop Set.empty 0 0

let instructions = 
    Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
    |> File.ReadAllLines
    |> Seq.map parseInstruction
    |> Array.ofSeq


// 1179
let part1 = eval instructions

// 1089
let part2 =
    instructions
    |> Seq.indexed
    |> Seq.filter (snd >> function Acc _ -> false | _ -> true)
    |> Seq.map (fun (i, instr) ->
        let copy = Array.copy instructions
        copy.[i] <- match instr with | Jmp x -> Nop x | Nop x -> Jmp x
        copy
    )
    |> Seq.choose (eval >> function NoLoop x -> Some x | _ -> None)
    |> Seq.head
