#load "Common.fsx"

open AdventCommon
open System
open System.IO

let testinput1 = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"

let matchcount (card:string) =
    let sections = card.Split([|':';'|'|], StringSplitOptions.RemoveEmptyEntries)
    let winning = sections.[1].Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
    let got = sections.[2].Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
    got |> Array.filter (fun n -> Array.contains n winning) |> Array.length

let score (card:string) =
    let matchcount' = matchcount card
    Math.Pow(2.0, float (matchcount' - 1)) |> int

let part1 (cards:seq<string>) = cards |> Seq.map score |> Seq.sum

let testoutput1 = testinput1 |> getlines |> part1
printfn "Test part 1 output: %d" testoutput1

let input1 = File.ReadAllLines("Day4.txt")
let output1 = input1 |> part1
printfn "Part 1 output: %d" output1

let part2 (cards:seq<string>) =
    //this is hard with immutable
    let matchcounts = cards |> Seq.map matchcount |> Array.ofSeq
    let count = Array.length matchcounts
    //each card counts, even if zero matches: so the array contains all cards whatever
    let scores = Array.init count (fun _ -> 1)
    for i in 0 .. count - 1 do
        for j in 1 .. matchcounts.[i] do
           //for each card, the number of cards it wins is added to the existing instances of that card
           scores.[i + j] <- scores.[i + j] + scores.[i]
    Array.sum scores

let testoutput2 = testinput1 |> getlines |> part2
printfn "Test part 2 output: %d" testoutput2

let output2 = input1 |> part2
printfn "Part 2 output: %d" output2