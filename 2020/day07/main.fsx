open System.IO
open System.Text.RegularExpressions

type Bag = { Kind: string; Color: string }
type ChildRule = int * Bag
type Rule = Bag * ChildRule list

let inline captured (m: Match) (i: int) = m.Groups.[i].Value

let parseChildRule =
    let regex = Regex(@"^(\d+) (\w+) (\w+) bags?$")

    fun input ->
        let m = regex.Match(input)

        (captured m 1 |> int,
         { Kind = captured m 2
           Color = captured m 3 })

let parseRule =
    let regex = Regex(@"(\w+) (\w+) bags contain (.*)\.")

    fun input ->
        let m = regex.Match(input)

        let children =
            match captured m 3 with
            | "no other bags" -> []
            | s ->
                s.Split(", ")
                |> Seq.map parseChildRule
                |> List.ofSeq

        ({ Kind = captured m 1
           Color = captured m 2 },
         children)

let rules =
    Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
    |> File.ReadAllLines
    |> Seq.map parseRule
    |> List.ofSeq

// 355
let part1 =
    let findContainersFor bag =
        rules
        |> Seq.filter (fun (_, children) -> children |> Seq.exists (fun (_, b) -> b = bag))
        |> Seq.map fst

    // Like a breadth-first graph search:
    // - start from "shiny gold" entry point
    // - traverse all bags which can be its containers
    // - keep a queue of nodes yet to be visited
    // - keep a set of nodes already visited
    let buildNewState (visited, queue) container =
        if Set.contains container visited then
            visited, queue
        else
            Set.add container visited, container :: queue

    let rec loop visited = function
    | [] -> visited
    | bag :: queue ->
        let (newVisited, newQueue) = 
            bag
            |> findContainersFor
            |> Seq.fold buildNewState (visited, queue)

        loop newVisited newQueue

    loop Set.empty [ { Kind = "shiny"; Color = "gold" } ]
    |> Set.count

// Self-recursive memoization
let memoized fn =
    let mutable memo = Map.empty

    let rec memoized input =
        match Map.tryFind input memo with
        | Some x -> x
        | None ->
            let value = fn memoized input
            memo <- Map.add input value memo
            value

    memoized

// 5312
let part2 =
    let directChildrenOf bag =
        rules
        |> Seq.filter (fun (b, _) -> b = bag)
        |> Seq.collect snd

    memoized
        (fun recursion bag -> directChildrenOf bag |> Seq.sumBy (fun (count, child) -> (recursion child + 1) * count))
        {Kind = "shiny"; Color = "gold"}
