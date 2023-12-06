#load "Common.fsx"

open AdventCommon
open System
open System.IO

let zero = (int)'0'

let part1 (input:string[]) =
        input |> Array.map (fun l -> l |> Seq.filter Char.IsDigit)
              |> Array.map (fun s -> (Seq.item 0 s), (Seq.last s))
              |> Array.map (fun (f,l) -> (int f) - zero, (int l) - zero)
              |> Array.map (fun (f,l) -> 10 * f + l)

let test1input = "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet"

let test1output = test1input |> getlines |> part1 |> Array.sum

printfn "Test part 1 output: %d" test1output

let input1 = File.ReadAllLines("Day1.txt")
let output1 = input1 |> part1 |> Array.sum

printfn "Output part 1: %d" output1

let test2input = "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen"

let numbers = ["zero";"one";"two";"three";"four";"five";"six";"seven";"eight";"nine"]
let digits = ["0";"1";"2";"3";"4";"5";"6";"7";"8";"9"]

//need to find first and last index of numbers in each row

let lineHack (line:string) =
    let words = numbers |> Seq.mapi (fun index number -> line.IndexOf(number), index)
    let digits' = digits |> Seq.mapi (fun index number -> line.IndexOf(number), index)
    let first = Seq.append words digits' |> Seq.filter (fun (pos, _) -> pos >= 0) |> Seq.minBy fst |> snd
    let words = numbers |> Seq.mapi (fun index number -> line.LastIndexOf(number), index)
    let digits' = digits |> Seq.mapi (fun index number -> line.LastIndexOf(number), index)
    let last = Seq.append words digits' |> Seq.filter (fun (pos, _) -> pos >= 0) |> Seq.maxBy fst |> snd
    (10 * first + last)

let part2 (input:string[]) = input |> Array.map lineHack

let test2output = test2input |> getlines |> part2 |> Array.sum

printfn "Test part 2 output: %d" test2output

let input2 = File.ReadAllLines("Day1.txt")
let output2 = input2 |> part2 |> Array.sum

printfn "Output part 2: %d" output2