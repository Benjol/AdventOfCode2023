#load "Common.fsx"

open AdventCommon
open System.IO
open System.Text.RegularExpressions

let testinput1 = "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."

type Point = { X : int; Y : int }
type Number = { Value : int; TopLeft : Point; BottomRight: Point }
    with
        static member Create(value, y, startx, length) = { Value = value; TopLeft = { X = startx - 1; Y = y - 1}; BottomRight = { X = startx + length; Y = y + 1 } }
        member x.Contains point =
            x.TopLeft.X <= point.X && x.BottomRight.X >= point.X && x.TopLeft.Y <= point.Y && x.BottomRight.Y >= point.Y

let regex1 = Regex("\d+")
let regex2 = Regex("[^0-9\.]")

let numbers y (line:string) =
    let matches = regex1.Matches(line)
    matches |> Seq.map (fun m -> Number.Create(int m.Value, y, m.Index, m.Length))

let symbols y (line:string) =
    let matches = regex2.Matches(line)
    matches |> Seq.map (fun m -> { X = m.Index; Y = y })

let part1 (schematic:seq<string>) =
    let numbers' = schematic |> Seq.mapi (fun y line -> numbers y line) |> Seq.concat
    let symbols' = schematic |> Seq.mapi (fun y line -> symbols y line) |> Seq.concat
    let hassymbol (number:Number) = symbols' |> Seq.filter (fun symbol -> number.Contains symbol) |> Seq.isEmpty |> not
    let retained = numbers' |> Seq.filter hassymbol
    retained |> Seq.sumBy (fun number -> number.Value)

let testoutput1 = testinput1 |> Split2 '\r' '\n' |> part1
printfn "Test part 1 output: %d" testoutput1

let input1 = File.ReadAllLines("Day3.txt")
let output1 = input1 |> part1
printfn "Part 1 output: %d" output1

let regex3 = Regex("\*")

let gears y (line:string) =
    let matches = regex3.Matches(line)
    matches |> Seq.map (fun m -> { X = m.Index; Y = y })

let part2 (schematic:seq<string>) =
    let numbers' = schematic |> Seq.mapi (fun y line -> numbers y line) |> Seq.concat
    let gears' = schematic |> Seq.mapi (fun y line -> gears y line) |> Seq.concat
    let gearratio (gear:Point) =
        let gearnumbers = numbers' |> Seq.filter (fun number -> number.Contains gear) |> List.ofSeq
        match gearnumbers with
        | [first;second] -> first.Value * second.Value
        | _ -> 0

    gears' |> Seq.sumBy gearratio

let testoutput2 = testinput1 |> Split2 '\r' '\n' |> part2
printfn "Test part 2 output: %d" testoutput2

let output2 = input1 |> part2
printfn "Part 2 output: %d" output2