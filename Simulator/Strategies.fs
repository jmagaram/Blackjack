module Strategies

open Cards.Patterns
open Card.Patterns

module BasicStrategy =

    // For dealer hits on soft 17
    // https://wizardofodds.com/games/blackjack/strategy/4-decks/

    type DesiredAction =
        | H // Hit
        | S // Stand
        | Dh // Double if allowed, otherwise hit
        | Ds // Double if allowed, otherwise stand
        | P // Split
        | Ph // Split if double after split is allowed, otherwise hit
        | Rh // Surrender if allowed, otherwise hit
        | Rs // Surrender if allowed, otherwise stand
        | Rp // Surrender if allowed, otherwise stand

    type DealerCard = DealerCard of Card

    type DecisionTable = Cards -> DealerCard -> DesiredAction
    let decisionTable : DecisionTable = fun cards (DealerCard dealer) ->
        match cards, dealer |> Card.minValue with
        | ((PairOf 2) | (PairOf 3)), (2|3) -> Ph 
        | PairOf 4, (2|3|4|7|8|9|10|1) -> H 
        | PairOf 4, (5|6) -> Ph 
        | PairOf 6, 2 -> Ph 
        | PairOf 6, (3|4|5|6) -> P 
        | PairOf 6, (7|8|9|10|1) -> H 
        | PairOf 7, (2|3|4|5|6|7) -> P 
        | PairOf 7, (8|9|10|1) -> H 
        | PairOf 8, (2|3|4|5|6|7|8|9|10) -> P 
        | PairOf 8, 1 -> Rp 
        | PairOf 9, (2|3|4|5|6|8|9) -> P 
        | PairOf 9, (7|10|1) -> S 
        | PairOf 1, _ -> P 
        | Hard x, _ when x<=8  -> H
        | Hard 9, (2|7|8|9|10|1) -> H
        | Hard 9, (3|4|5|6) -> Dh
        | Hard 10, (2|3|4|5|6|7|8|9) -> Dh
        | Hard 10, (10|1) -> H
        | Hard 11, _ -> Dh
        | Hard 12, (2|3|7|8|9|10|1) -> H
        | Hard 12, (4|5|6) -> S
        | ((Hard 13) | (Hard 14)), (2|3|4|5|6) -> S
        | ((Hard 13) | (Hard 14)), (7|8|9|10|1) -> H
        | Hard 15, (2|3|4|5|6) -> S
        | Hard 15, (7|8|9) -> H
        | Hard 15, (10|1) -> Rh
        | Hard 16, (2|3|4|5|6) -> S
        | Hard 16, (7|8) -> H
        | Hard 16, (9|10|1) -> Rh
        | Hard 17, (2|3|4|5|6|7|8|9|10) -> S
        | Hard 17, 1 -> Rs
        | Hard x, _ when x>=18 -> S
        | ((Soft 13) | (Soft 14)), (2|3|4|7|8|9|10|1) -> H
        | ((Soft 13) | (Soft 14)), (5|6) -> Dh
        | ((Soft 15) | (Soft 16)), (2|3|7|8|9|10|1) -> H
        | ((Soft 15) | (Soft 16)), (4|5|6) -> Dh
        | Soft 17, (2|7|8|9|10|1) -> H
        | Soft 17, (3|4|5|6) -> Dh
        | Soft 18, (2|3|4|5|6) -> Ds
        | Soft 18, (7|8) -> S
        | Soft 18, (9|10|1) -> H
        | Soft 19, (2|3|4|5|7|8|9|10|1) -> S
        | Soft 19, 6 -> Ds
        | Soft x, _ when x >=20 -> S
        | _, _ -> failwith "Did not specify all the match patterns."

    let surrender : SurrenderStrategy = fun ctx ->
        match decisionTable ctx.PlayerCards (DealerCard ctx.DealerFaceUpCard) with
        | H -> KeepPlaying
        | S -> KeepPlaying
        | Dh -> KeepPlaying
        | Ds -> KeepPlaying
        | P -> KeepPlaying
        | Ph -> KeepPlaying
        | Rh -> SurrenderHand
        | Rs -> SurrenderHand
        | Rp -> SurrenderHand

    let insurance : InsuranceStrategy = fun ctx -> DeclineInsurance

    let split : SplitStrategy = fun ctx -> 
        match decisionTable ctx.PlayerCards (DealerCard ctx.DealerFaceUpCard) with
        | H -> DoNotSplitHand
        | S -> DoNotSplitHand
        | Dh -> DoNotSplitHand
        | Ds -> DoNotSplitHand
        | P -> SplitHand
        | Ph -> SplitHand
        | Rh -> DoNotSplitHand
        | Rs -> DoNotSplitHand
        | Rp -> SplitHand

    let double : DoubleDownStrategy = fun ctx -> 
        match decisionTable ctx.PlayerCards (DealerCard ctx.DealerFaceUpCard) with
        | H -> DoNotDoubleDown
        | S -> DoNotDoubleDown
        | Dh -> DoubleDownHand
        | Ds -> DoubleDownHand
        | P -> DoNotDoubleDown
        | Ph -> DoNotDoubleDown
        | Rh -> DoNotDoubleDown
        | Rs -> DoNotDoubleDown
        | Rp -> DoNotDoubleDown

    let hit : HitStrategy = fun ctx -> 
        match decisionTable ctx.PlayerCards (DealerCard ctx.DealerFaceUpCard) with
        | H -> HitHand
        | S -> Stand
        | Dh -> HitHand
        | Ds -> Stand
        | P -> Stand
        | Ph -> HitHand
        | Rh -> HitHand
        | Rs -> Stand
        | Rp -> Stand

    let alwaysBetMinimum : BuyInStrategy = fun _ -> Some 1m<factor>

    let create playerName = 
        { Player.PlayerName = playerName 
          CountingSystem = None
          BuyInStrategy = alwaysBetMinimum
          InsuranceStrategy = insurance
          SurrenderStrategy = surrender
          SplitStrategy = split
          DoubleDownStrategy = double 
          HitStrategy = hit }

module HighLow =

    let cardValue card =
        match card with
        | MinCardValue x when x>=2 && x<=6 -> 1
        | MinCardValue 10 -> -1
        | MinCardValue 1 -> -1
        | _ -> 0

    type Statistics =
        { HighLowCount : int
          TotalCardsSeen : int<cardQty>
          TotalCards : int<cardQty> }
        member this.TrueCount() =
            let cardsRemaining = this.TotalCards - this.TotalCardsSeen
            let decksRemaining =  (Measured.toMeasuredDecimal cardsRemaining) / 52m<cardQty/deck>
            (decimal this.HighLowCount) / decksRemaining * 1m<deck>

    let countOneCard card stats = 
        { stats with
            HighLowCount = stats.HighLowCount + cardValue card
            TotalCardsSeen = stats.TotalCardsSeen + 1<cardQty> }

    let highLowKey = "HighLowKey"
    let totalCardsSeenKey = "TotalCardsSeen"
    let trueCountKey = "TrueCount"
    let totalCardsKey = "TotalCards"

    let convertToGenericCount stats = 
        Map.empty
        |> Map.add highLowKey (decimal stats.HighLowCount)
        |> Map.add totalCardsSeenKey (decimal stats.TotalCardsSeen)
        |> Map.add totalCardsKey (decimal stats.TotalCards)
        |> CardCount

    let convertFromGenericCount (CardCount count) =
        { HighLowCount = int (Map.find highLowKey count)
          TotalCardsSeen = (int (Map.find totalCardsSeenKey count)) * 1<cardQty> 
          TotalCards = (int (count |> Map.find totalCardsKey)) *1<cardQty> }

    type HighLow =
        { GetStatistics : CardCount -> Statistics      
          System : CountingSystem }

    let create (totalCards:int<cardQty>) =
        let startCountAt =
            { HighLowCount = 0
              TotalCardsSeen = 0<cardQty> 
              TotalCards = totalCards }
        { HighLow.System = 
            { CountingSystem.Start = 
                startCountAt 
                |> convertToGenericCount
              CountingSystem.CountCard = 
                fun card cnt -> 
                    cnt 
                    |> convertFromGenericCount 
                    |> countOneCard card 
                    |> convertToGenericCount }
          GetStatistics = convertFromGenericCount }