#load "Common.fsx"

open AdventCommon
open System.IO

let testinput1 = "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4"

type Range = { InStart:int64; OutStart:int64; Length:int64 }
    with static member Parse (input:string) =
            let arr = input.Split([|' '|]) |> Array.map int64
            { InStart = arr.[1]; OutStart = arr.[0]; Length = arr.[2] }
         member x.Match value =
            let diff = value - x.InStart
            if diff >= 0l && diff < x.Length then
                Some (x.OutStart + diff)
            else
                None
         member x.MatchBack value =
            let diff = value - x.OutStart
            if diff >= 0l && diff < x.Length then
                Some(x.InStart + diff)
            else
                None

type Mapping = { From:string; To:string; Ranges: Range list }
    with static member Parse (input:string) =
            let arr = input.Split("\n")
            let ranges = arr |> Array.skip 1 |> Array.map Range.Parse |> List.ofArray
            let names = arr.[0].Split([|'-';' '|])
            { From = names.[0]; To = names.[2]; Ranges = ranges }
         member x.Convert value =
            match x.Ranges |> List.choose (fun r -> r.Match value) with
            | converted::_ -> converted
            | [] -> value
         member x.ConvertBack value =
            match x.Ranges |> Seq.choose (fun r -> r.MatchBack value) |> Seq.tryItem 0 with
            | Some(converted) -> converted
            | None -> value

let part1 (input:string) =
    let parts = input.Split("\n\n")
    let seeds = parts.[0].Split([|' '|]) |> Array.skip 1 |> Array.map int64
    let maps = parts |> Array.skip 1 |> Array.map Mapping.Parse |> Array.map (fun m -> m.From, m) |> Map.ofSeq
    let rec map key value =
        match Map.tryFind key maps with
        | Some(mapping) ->
            map mapping.To (mapping.Convert(value))
        | None -> value
    let locations = seeds |> Array.map (fun seed -> map "seed" seed)
    locations |> Array.min

let testoutput1 = testinput1 |> part1
printfn "Test part 1 output: %A" testoutput1

let input1 = File.ReadAllText("Day5.txt")
let output1 = input1 |> part1
printfn "Part 1 output: %A" output1

let part2 (input:string) =
    let parts = input.Split("\n\n")
    let seedranges = parts.[0].Split([|' '|])
                        |> Array.skip 1
                        |> Array.map int64
                        |> Array.chunkBySize 2
                        |> Array.map (fun arr -> arr.[0], arr.[1])

    let inrange seed =
        seedranges |> Array.exists (fun (start,length) -> seed >= start && seed <= start + length)

    let maps = parts |> Array.skip 1 |> Array.map Mapping.Parse |> Array.map (fun m -> m.To, m) |> Map.ofSeq
    let rec mapBack key value =
        match Map.tryFind key maps with
        | None -> value
        | Some(mapping) ->
            mapBack mapping.From (mapping.ConvertBack(value))

    let locations = Seq.initInfinite (fun i -> int64 i)

    locations |> Seq.map (fun location -> location, mapBack "location" location )
              |> Seq.filter (fun (_,seed) -> inrange seed)
              |> Seq.item 0
              |> fst

let testoutput2 = testinput1 |> part2
printfn "Test part 2 output: %A" testoutput2

let output2 = input1 |> part2
printfn "Part 2 output: %A" output2