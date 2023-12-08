#load "Common.fsx"

open AdventCommon
open System.IO
open System.Text.RegularExpressions

let testinput = "32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483"

let cards = ['A'; 'K'; 'Q'; 'J'; 'T'; '9'; '8'; '7'; '6'; '5'; '4'; '3'; '2'] |> List.rev |> List.mapi (fun i c -> c, i) |> dict

type Hand = { Cards : string; Bid: int }
    with static member Parse str =
                let arr = str |> Split ' '
                { Cards = arr.[0]; Bid = int arr.[1] }
type HandType =
    | FiveOfAKind = 6  //AAAAA
    | FourOfAKind = 5  //AAAAB
    | FullHouse = 4    //AABBB
    | ThreeOfAKind = 3 //AAABC
    | TwoPair = 2      //AABBC
    | OnePair = 1      //AABCD
    | HighCard = 0     //ABCDE

let getType withJokers hand =
    match hand.Cards.ToCharArray() |> Array.groupBy id |> Array.sortByDescending (fun (_,grp) -> grp.Length) with
    | grp when grp.Length = 1 -> HandType.FiveOfAKind
    | grp when grp.Length = 2 ->
        match grp.[0] with
        | (c,set) when set.Length = 4 -> HandType.FourOfAKind
        | _ -> HandType.FullHouse
    | grp when grp.Length = 3 ->
        match grp.[0] with
        | (c,set) when set.Length = 3 -> HandType.ThreeOfAKind
        | _ -> HandType.TwoPair
    | grp when grp.Length = 4 -> HandType.OnePair
    | _ -> HandType.HighCard

let getRank hand = hand.Cards |> Seq.map (fun c -> cards.[c]) |> Seq.mapi (fun i r -> r <<< (4 * (4 - i))) |> Seq.sum

let part1 (games:seq<string>) =
       games
        |> Seq.map Hand.Parse
        |> Seq.sortBy (fun hand -> getType false hand, getRank hand)
        //|> Seq.mapi (fun rank hand -> printfn "%A %s %A %020B %A" (rank + 1) hand.Cards (getType false hand) (getRank hand) ((rank + 1) * hand.Bid); hand)
        |> Seq.mapi (fun rank hand -> (rank + 1) * hand.Bid)
        |> Seq.sum

let testoutput1 = testinput |> Split2 '\r' '\n' |> part1
printfn "Test part 1 output: %A" testoutput1

let input1 = File.ReadAllLines("Day7.txt")
let output1 = input1 |> part1
printfn "Part 1 output: %A" output1


let part2 (games:seq<string>) = ""

let testoutput2 = testinput |> Split2 '\r' '\n' |> part2
printfn "Test part 2 output: %A" testoutput2

let output2 = input1 |> part2
printfn "Part 2 output: %A" output2