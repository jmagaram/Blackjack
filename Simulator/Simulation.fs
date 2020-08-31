module Simulation

open Shoe.Patterns

let start : Simulation = fun rules seed players ->
    let shuffler = Shoe.shuffler seed rules.DealDepth rules.ShoeComposition
    let counts = Counts.initialize players
    let shoe = shuffler()
    let generator (shoe, counts) = 
        let (shoe, counts) =
            match shoe with
            | ShuffleRequired -> (shuffler(), counts |> Counts.newShoe)
            | CutCardNotYetReached -> (shoe, counts)
        let round = ActiveRound.start counts shoe players rules
        let (shoe, counts, completed) =
            round
            |> ActiveRound.offerInsurance
            |> ActiveRound.stopHandsIfDealerHasBlackjack
            |> ActiveRound.playRemainingActiveHands
            |> ActiveRound.revealDealerHoleCard
            |> ActiveRound.playDealerHand
            |> ActiveRound.resolveBets
            |> function 
                r -> 
                let completedRound =
                    { CompletedHands = r.PlayerHands
                      Dealer = r.Dealer }
                (r.Shoe, r.Counts, completedRound)
        let state = (shoe, counts)
        let item = completed
        Some (item, state)
    Seq.unfold generator (shoe, counts)
