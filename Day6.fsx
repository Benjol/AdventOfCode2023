#load "Common.fsx"

open AdventCommon
open System
open System.IO
open LanguagePrimitives

let testinput = "Time:      7  15   30
Distance:  9  40  200"

[<Measure>] type mm
[<Measure>] type ms

let ChargingRate = 1.0<mm/ms^2>

let inline previous (x: float<'a>) =
    match x |> float |> floor with
    | f when f = float x -> x - (FloatWithMeasure 1.)
    | f -> FloatWithMeasure f

let inline next (x: float<'a>) =
    match x |> float |> ceil with
    | c when c = float x -> x + (FloatWithMeasure 1.)
    | c -> FloatWithMeasure c

type Race = { RaceTime: float<ms>; BestDistance: float<mm> }
    with member x.WinningOptions =
          let a = -ChargingRate
          let b = x.RaceTime * ChargingRate
          let c = -x.BestDistance
          let Δ = sqrt(b * b - 4. * a * c)
          let x1 = (-b + Δ)/(2. * a)
          let x2 = (-b - Δ)/(2. * a)
          let minTime = next x1
          let maxTime = previous x2
          //printfn "%A %A %A %A" x1 x2 minTime maxTime
          1 + int(maxTime - minTime)

//Speed = ChargingTime * ChargingRate
//RaceDistance = RunTime * Speed
//RaceDistance = BestDistance
//RaceTime = CharginTime + RunTime
//so:
//RunTime = RaceTime - ChargingTime
//ChargingTime * ChargingRate * (RaceTime - ChargingTime) = BestDistance
//ChargingTime * ChargingRate * RaceTime - ChargingTime * ChargingRate * ChargingTime = BestDistance
//ChargingTime * ChargingRate * RaceTime - ChargingTime * ChargingRate * ChargingTime - BestDistance = 0
//-ChargingRate * ChargingTime^2 + RaceTime * ChargingRate * ChargingTime - BestDistance = 0
//ChargingTime as x:
//ax^2 + bx + c = 0
//x = -b ± sqrt(b^2 - 4ac)/2a
//Δ = sqrt(b^2 - 4ac)/2a = sqrt(RaceTime * ChargingRate * RaceTime * ChargingRate - 4 * ChargingRate * BestDistance)
//x1 = (-RaceTime * ChargingRate + Δ)/(2 * -ChargingRate)
//x2 = (-RaceTime * ChargingRate - Δ)/(2 * -ChargingRate)
//inverted parabolic curve, left side at origin
// races which go further than BestDistance have ChargingTimes between x1 and x2

let part1 (input:seq<string>) =
    let arr = input |> Array.ofSeq
    let times = arr.[0] |> Split ' ' |> Array.skip 1 |> Array.map float |> Array.map FloatWithMeasure
    let distances = arr.[1] |> Split ' ' |> Array.skip 1 |> Array.map float |> Array.map FloatWithMeasure
    let races = Array.map2 (fun time distance -> { RaceTime = time; BestDistance = distance }) times distances

    races |> Array.map (fun race -> race.WinningOptions) |> Array.reduce (*)

let testoutput1 = testinput |> Split2 '\r' '\n' |> part1
printfn "Test part 1 output: %A" testoutput1

let input1 = File.ReadAllLines("Day6.txt")
let output1 = input1 |> part1
printfn "Part 1 output: %A" output1

let part2 (input:seq<string>) =
    let arr = input |> Array.ofSeq
    let times = arr.[0] |> Split ':' |> Array.item 1
    let time = times.Replace(" ", "") |> float |> FloatWithMeasure
    let distances = arr.[1] |> Split ':' |> Array.item 1
    let distance = distances.Replace(" ", "") |> float |> FloatWithMeasure
    let race = { RaceTime = time; BestDistance = distance }
    race.WinningOptions

let testoutput2 = testinput |> Split2 '\r' '\n' |> part2
printfn "Test part 2 output: %d" testoutput2

let output2 = input1 |> part2
printfn "Part 2 output: %d" output2