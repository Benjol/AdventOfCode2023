#load "Common.fsx"

open AdventCommon
open System.IO
open System.Collections.Generic
open System.Text.RegularExpressions

let testinput = "RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)"

let regex = Regex("[A-Z1-2]{3}")
type Instruction = { Id: string; Left : string; Right : string }
    with
        static member Parse txt =
            let [id;left;right] = regex.Matches(txt) |> List.ofSeq
            { Id = id.Value; Left = left.Value;Right = right.Value }
type Direction =
    | Left
    | Right

type Instructions = { Directions : seq<Direction>; Network : Map<string,Instruction> }
    with
        static member Parse txt =
            let arr = txt |> Split2 '\r' '\n'
            let directions =
                let inner = arr.[0].ToCharArray() |> Array.map (fun c -> if c = 'L' then Left else Right)
                Seq.initInfinite (fun i -> inner.[i % inner.Length])
            let network = arr |> Array.skip 1 |> Array.map Instruction.Parse |> Array.map (fun i -> i.Id, i) |> Map.ofArray
            { Directions = directions; Network = network }

let length instructions station endcondition =
    instructions.Directions
        |> Seq.scan (fun (station,steps) direction ->
                            let instruction = Map.find station instructions.Network
                            match direction with
                            | Left -> instruction.Left, steps + 1
                            | Right -> instruction.Right, steps + 1) (station, 0)

        |> Seq.find (fun (station, _) -> endcondition station)
        |> snd

let part1 (input) =
    let instructions = input |> Instructions.Parse
    length instructions "AAA" (fun s -> s = "ZZZ")

let testoutput1 = testinput |> part1
printfn "Test part 1 output: %A" testoutput1

let testinput2 = "LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)"
let testoutput1b = testinput2 |> part1
printfn "Test part 1b output: %A" testoutput1b


let input1 = File.ReadAllText("Day8.txt")
let output1 = input1 |> part1
printfn "Part 1 output: %A" output1

let testinput3 = "LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)"

let part2 (input) =
    let instructions = input |> Instructions.Parse
    let starts = instructions.Network.Keys |> Seq.filter (fun n -> n.EndsWith("A"))

    let lengths = starts |> Seq.map (fun station -> length instructions station (fun s -> s.EndsWith("Z")))

    let rec gcd x y =
        if y = 0L then x
        else gcd y (x % y)
    let rec lcm2 x y = abs (x * y) / (gcd x y)
    let rec lcm nums =
        match nums with
        | [] -> 0L
        | [x;y] -> lcm2 x y
        | x::tail -> lcm2 x (lcm tail)
    lengths |> Seq.iter (printfn "%A")
    lengths |> Seq.map int64 |> List.ofSeq |> lcm

let testoutput2 = testinput3 |> part2
printfn "Test part 2 output: %A" testoutput2

let output2 = input1 |> part2
printfn "Part 2 output: %A" output2