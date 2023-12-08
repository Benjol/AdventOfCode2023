#load "Common.fsx"

open AdventCommon
open System.IO
open System.Collections.Generic

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
    let jokers = match withJokers with | true -> hand.Cards.ToCharArray() |> Array.filter ((=) 'J') |> Array.length | false -> 0
    //with 1 joker and 3 groups it's FourOfAKind or FullHouse   JAAAB, JAABB
    //with 0 joker it's same as before
    let groups = hand.Cards.ToCharArray() |> Array.groupBy id |> Array.sortByDescending (fun (_,grp) -> grp.Length)
    match (jokers, groups.Length) with
    | (_, 1) //XXXXX
    | (5, _) //JJJJJ
    | (4, _) //JJJJA
    | (3, 2) //JJJAA
    | (2, 2) //JJAAA
    | (1, 2) //JAAAA
        -> HandType.FiveOfAKind
    | (3, 3) //JJJAB
    | (2, 3) //JJAAB
        -> HandType.FourOfAKind
    | (2, 4) //JJABC
    | (1, 4) //JAABC
        -> HandType.ThreeOfAKind
    | (1, 5) //JABCD
    | (_, 4) //AABCD
        -> HandType.OnePair
    | (1, 3) ->
        match groups.[0] with
        | (_,set) when set.Length = 3 -> HandType.FourOfAKind  //JAAAB
        | _ -> HandType.FullHouse                              //JAABB
    | (_, 2) ->
        match groups.[0] with
        | (_,set) when set.Length = 4 -> HandType.FourOfAKind  //AAAAB
        | _ -> HandType.FullHouse                              //AABBB
    | (_, 3)->
        match groups.[0] with
        | (_,set) when set.Length = 3 -> HandType.ThreeOfAKind //AAABC
        | _ -> HandType.TwoPair
    | _ -> HandType.HighCard

let getRank (lookup:IDictionary<char,int>) hand = hand.Cards |> Seq.map (fun c -> lookup.[c]) |> Seq.mapi (fun i r -> r <<< (4 * (4 - i))) |> Seq.sum

let part1 (games:seq<string>) =
       games
        |> Seq.map Hand.Parse
        |> Seq.sortBy (fun hand -> getType false hand, getRank cards hand)
        //|> Seq.mapi (fun rank hand -> printfn "%A %s %A %020B %A" (rank + 1) hand.Cards (getType false hand) (getRank hand) ((rank + 1) * hand.Bid); hand)
        |> Seq.mapi (fun rank hand -> (rank + 1) * hand.Bid)
        |> Seq.sum

let testoutput1 = testinput |> Split2 '\r' '\n' |> part1
printfn "Test part 1 output: %A" testoutput1

let input1 = File.ReadAllLines("Day7.txt")
let output1 = input1 |> part1
printfn "Part 1 output: %A" output1


let part2 (games:seq<string>) =
       let cards' = ['A'; 'K'; 'Q'; 'T'; '9'; '8'; '7'; '6'; '5'; '4'; '3'; '2'; 'J'] |> List.rev |> List.mapi (fun i c -> c, i) |> dict
       games
        |> Seq.map Hand.Parse
        |> Seq.sortBy (fun hand -> getType true hand, getRank cards' hand)
        //|> Seq.mapi (fun rank hand -> printfn "%A %s %A %020B %A" (rank + 1) hand.Cards (getType true hand) (getRank hand) ((rank + 1) * hand.Bid); hand)
        |> Seq.mapi (fun rank hand -> (rank + 1) * hand.Bid)
        |> Seq.sum

let testoutput2 = testinput |> Split2 '\r' '\n' |> part2
printfn "Test part 2 output: %A" testoutput2

let output2 = input1 |> part2
printfn "Part 2 output: %A" output2