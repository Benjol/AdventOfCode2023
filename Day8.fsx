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

let regex = Regex("[A-Z]{3}")
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

let part1 (input) =
    let instructions = input |> Instructions.Parse
    instructions.Directions
        |> Seq.scan (fun (station,steps) direction ->
                            let instruction = Map.find station instructions.Network
                            match direction with
                            | Left -> instruction.Left, steps + 1
                            | Right -> instruction.Right, steps + 1) ("AAA", 0)

        |> Seq.find (fun (station, _) -> station = "ZZZ")
        |> snd

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

//let part2 (games:seq<string>) = ""

//let testoutput2 = testinput |> Split2 '\r' '\n' |> part2
//printfn "Test part 2 output: %A" testoutput2

//let output2 = input1 |> part2
//printfn "Part 2 output: %A" output2