#load "Common.fsx"

open AdventCommon
open System.IO
open System.Collections.Generic
open System.Text.RegularExpressions

let testinput = "0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45"

let predict numbers =
    let diffs nbs = nbs |> Array.pairwise |> Array.map (fun (a,b) -> b - a)
    let zeros nbs = nbs |> Seq.forall ((=) 0)
    let rec xx nbs =
        match zeros nbs with
        | true -> 0
        | _ -> (Array.last nbs) + (nbs |> diffs |> xx)
    xx numbers

let part1 (input) =
    input |> Split2 '\n' '\r'
          |> Seq.map (Split ' ')
          |> Seq.map (Array.map int)
          |> Seq.map predict
          |> Seq.sum

let testoutput1 = testinput |> part1
printfn "Test part 1 output: %A" testoutput1


let input1 = File.ReadAllText("Day9.txt")
let output1 = input1 |> part1
printfn "Part 1 output: %A" output1

let part2 (input) =
    input |> Split2 '\n' '\r'
          |> Seq.map (Split ' ')
          |> Seq.map Array.rev
          |> Seq.map (Array.map int)
          |> Seq.map predict
          |> Seq.sum

let testoutput2 = testinput |> part2
printfn "Test part 2 output: %A" testoutput2

let output2 = input1 |> part2
printfn "Part 2 output: %A" output2