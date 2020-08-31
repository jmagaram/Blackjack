module Formatters

open Bet.Patterns
open Cards.Patterns
open System.Text

let chipsDelta (b:decimal<chips>) = 
    match b with
    | 0m<chips> -> sprintf "%0.1f" b
    | x -> sprintf "%0+.1f" b

let chipsAbs (b:decimal<chips>) = 
    sprintf "%0.1f" (abs b)

let bet (b:Bet) =
    match b with
    | Zero -> "no bet"
    | Push -> "push"
    | WinEven -> b.Profit |> chipsDelta
    | LoseEven -> b.Profit |> chipsDelta
    | WinMultiple -> sprintf "%s on %s" (b.Profit |> chipsDelta) (b.Wagered |> chipsAbs)
    | LoseMultiple -> sprintf "%s on %s" (b.Profit |> chipsDelta) (b.Wagered |> chipsAbs)

let betGain (b:Bet) = sprintf "%.2f%%" ((b |> Bet.gain) * 100m)

let cardsSummaryOnly (cards:Cards) =
    match cards with
    | Blackjack -> "BJ"
    | Bust ->      "~~"
    | Score s -> sprintf "%2i" s

let cards (cards:Cards) =
    cards.Cards
    |> Seq.rev
    |> Seq.map (fun i -> i.Compact)
    |> Seq.reduce (fun a b -> sprintf "%s %s" a b)
    |> function x -> sprintf "[%s] %s" (cardsSummaryOnly cards) x 

let completedHand (h:CompletedHand) =
    let cards = h.Cards |> cards
    let mainBet = h.Bet |> bet |> sprintf "| %s"
    let insurance = h.Insurance |> Option.map bet |> Option.map (sprintf "| %s") |> Option.defaultValue ""
    let adjust =
        h.Adjustment
        |> Option.map (fun a -> 
            match a with
            | BetAdjustment.DoubledDown -> "| doubled"
            | BetAdjustment.Surrendered -> "| surrender")
        |> Option.defaultValue ""
    sprintf "%s %s %s %s" cards mainBet insurance adjust

let completedRound (players:Players) (r:CompletedRound) =
    let result = new StringBuilder()
    let dealerName = "Dealer"
    let nameFormat = 
        players 
        |> Map.toSeq 
        |> Seq.map (fun (s,p)->p.PlayerName.Length)
        |> Seq.append (seq { yield dealerName.Length })
        |> Seq.max
        |> function len -> fun (n:string) -> n.PadRight(len)
    result.AppendLine (sprintf "%s -> %s" (dealerName |> nameFormat) (r.Dealer |> cards)) |> ignore
    r.CompletedHands 
    |> Map.toSeq
    |> Seq.sortBy (fun (hid, h) -> (hid.Seat, hid.Index))
    |> Seq.iter (fun (hid, h) -> 
        let playerName = 
            players 
            |> Map.find hid.Seat
            |> function p -> p.PlayerName |> nameFormat
        let handDetails = h |> completedHand
        let complete = sprintf "%s -> %s" playerName handDetails
        result.AppendLine(complete) |> ignore)
    result.ToString()

