#load "Common.fsx"

open AdventCommon
open System.IO
open System.Text.RegularExpressions

let testinput1 = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"

type Counts = { Red: int; Green: int; Blue: int}
    with
        static member Zero = { Red = 0; Green = 0; Blue = 0 }
        static member Max c1 c2 = { Red = max c1.Red c2.Red; Green = max c1.Green c2.Green; Blue = max c1.Blue c2.Blue }

let reds = Regex("(\d+) red")
let greens = Regex("(\d+) green")
let blues = Regex("(\d+) blue")

let count (set:string) =
    let countif (m:Match) = if m.Success then int m.Groups.[1].Value else 0
    let redcount = reds.Match(set) |> countif
    let greencount = greens.Match(set) |> countif
    let bluecount = blues.Match(set) |> countif
    { Red = redcount; Green = greencount; Blue = bluecount }

let passes counts (set:string) =
    let result = count set
    result.Red <= counts.Red && result.Green <= counts.Green && result.Blue <= counts.Blue

let gamesets (game:string) =
    let [|id; setlist|] = game |> Split ':'
    let sets = setlist |> Split ';'
    let number = id.Replace("Game ", "") |> int
    (number, sets)

let score counts (game:string) =
    let (number, sets) = gamesets game
    let pass = sets |> Array.forall (passes counts)
    if pass then
        number
    else
        0

let part1 counts (games:seq<string>) =
    games |> Seq.map (score counts) |> Seq.sum


let testoutput1 = testinput1 |> Split2 '\r' '\n' |> part1 { Red = 12; Green = 13; Blue = 14 }
printfn "Test part 1 output: %d" testoutput1

let input1 = File.ReadAllLines("Day2.txt")
let output1 = input1 |> part1 {Red = 12; Green = 13; Blue = 14 }
printfn "Part 1 output: %d" output1

let minimum (game:string) =
    let (_,sets) = gamesets game
    let counts = sets |> Array.map count
    counts |> Array.fold Counts.Max Counts.Zero

let part2 (games:seq<string>) =
    let scores = games |> Seq.map minimum
    let powers = scores |> Seq.map (fun c -> c.Red * c.Blue * c.Green)
    Seq.sum powers

let testoutput2 = testinput1 |> Split2 '\r' '\n' |> part2
printfn "Test part 2 output: %d" testoutput2

let output2 = input1 |> part2
printfn "Part 2 output: %d" output2