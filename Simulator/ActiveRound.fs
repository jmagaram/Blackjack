module ActiveRound

open Card.Patterns
open Cards.Patterns

let dealFaceUp counts shoe =
    let (card, shoe) = shoe |> Shoe.deal |> Seq.head
    let counts = counts |> Counts.countCard card
    (card, shoe, counts)

let createDealerHand shoe counts =
    let (faceUp, shoe, counts) = dealFaceUp counts shoe
    let (hole, shoe) = shoe |> Shoe.deal |> Seq.head
    let dealerHand = 
        { FaceUp = faceUp
          Hole = hole }
    (dealerHand, counts, shoe)

let start counts shoe players rules =
    let convertFactorToValidBet factor =
        let proposed = factor * rules.BetRange.MinInclusive / 1m<factor>
        let valid = rules.BetRange |> Range.coerce proposed
        valid      
    let buyInRequest (seat, player) =
        counts
        |> Counts.forSeat seat
        |> player.BuyInStrategy
        |> Option.map convertFactorToValidBet
        |> Option.map (fun b -> (seat, b))
    let createActiveOrStoppedHand (hands, shoe, counts) (seat, bet) =
        let (card1, shoe, counts) = shoe |> dealFaceUp counts
        let (card2, shoe, counts) = shoe |> dealFaceUp counts
        let cards = Cards.create card1 card2
        let hand =
            { ActiveHand.Cards = cards
              Insurance = None
              OriginalBet = bet }
            |> ActiveOrStoppedHand.Active
        let handId = 
            { Seat = seat
              Index = 0<hand> }
        let hands = hands |> Map.add handId hand
        (hands, shoe, counts)
    let (hands, shoe, counts) = 
        players
        |> Map.toSeq
        |> Seq.choose buyInRequest
        |> Seq.fold createActiveOrStoppedHand (Map.empty, shoe, counts)
    let (dealerHand, counts, shoe) = createDealerHand shoe counts
    { Round.PlayerHands = hands
      Shoe = shoe
      Counts = counts
      Players = players
      Rules = rules
      Dealer = dealerHand }

let decisionContext seat hand round =
    { Count = round.Counts |> Counts.forSeat seat
      DealerFaceUpCard = round.Dealer.FaceUp
      PlayerCards = hand.Cards }

let asActive hand =
    match hand with
    | Active a -> Some a
    | Stopped _ -> None

let asStopped hand =
    match hand with
    | Stopped s -> Some s
    | Active _ -> None

let tryFindActive (handId:HandId) (hands:Map<HandId,ActiveOrStoppedHand>) =
    hands
    |> Map.find handId
    |> Some
    |> Option.bind (fun h -> 
        h
        |> asActive
        |> Option.map (fun h -> (handId, h)))

let activeHands map =
    map
    |> Map.toSeq
    |> Seq.choose (fun (handId, hand) -> 
        hand 
        |> asActive 
        |> Option.map (fun hand -> (handId, hand)))

let stoppedHands map =
    map
    |> Map.toSeq
    |> Seq.choose (fun (handId, hand) -> 
        hand 
        |> asStopped 
        |> Option.map (fun hand -> (handId, hand)))

let nextHandId seat hands =
    { Seat = seat
      Index =
        hands
        |> Map.keys
        |> Seq.choose (fun handId -> if handId.Seat = seat then Some handId.Index else None)
        |> Seq.sort
        |> Seq.tryLast
        |> Option.map (fun h -> h + 1<hand>)
        |> Option.defaultValue 0<hand> }

let purchaseInsurance : PurchaseInsurance = fun h -> 
    { h with ActiveHand.Insurance = Some (h.OriginalBet / 2m) }

let stop (h:ActiveHand) =
    { StoppedHand.OriginalBet = h.OriginalBet 
      StoppedHand.BetAdjustment = None
      StoppedHand.Cards = h.Cards
      StoppedHand.Insurance = h.Insurance }

let stopSpecial adjustment (h:ActiveHand) =
    h
    |> stop
    |> function h -> { h with BetAdjustment = Some adjustment }

let stand : Stand = stop

let surrender : Surrender = stopSpecial BetAdjustment.Surrendered

let doubleDown : DoubleDown = fun card hand ->
    { hand with Cards = hand.Cards |> Cards.add card }
    |> stopSpecial BetAdjustment.DoubledDown

let split : Split = fun card1 card2 h ->
    let (c1, c2) = h.Cards |> Cards.split card1 card2
    let h1 = { h with ActiveHand.Cards = c1 }
    let h2 = { h with ActiveHand.Cards = c2 }
    (h1, h2)

let hit : Hit = fun card hand ->
    let hand = { hand with Cards = hand.Cards |> Cards.add card }
    match hand.Cards with
    | Bust -> hand |> stop |> ActiveOrStoppedHand.Stopped
    | _ -> hand |> ActiveOrStoppedHand.Active

let offerInsurance r =
    match r.Dealer.FaceUp with
    | Ace ->
        { r with 
            PlayerHands =
                r.PlayerHands
                |> activeHands
                |> Seq.choose (fun (hId, h) ->
                    match hId.Index with
                    | 0<hand> -> 
                        let player = r.Players |> Map.find hId.Seat
                        match 
                            r
                            |> decisionContext hId.Seat h
                            |> player.InsuranceStrategy with
                        | PurchaseInsurance -> Some (hId, h |> purchaseInsurance |> Active)
                        | DeclineInsurance -> None
                    | _ -> None)
                |> Seq.fold (fun hs (hId, h) -> hs |> Map.add hId h) r.PlayerHands }
    | _ -> r

let loseInsurance : LoseInsuranceIfAny = stop

let stopHandsIfDealerHasBlackjack r =
    match r.Dealer |> SecretDealerHand.isBlackjack with
    | true -> 
        let hands =
            r.PlayerHands
            |> Map.mapValues (fun h ->
                match h with
                | Stopped _ -> h
                | Active a -> a |> loseInsurance |> ActiveOrStoppedHand.Stopped )
        { r with PlayerHands = hands }
    | false -> r

let canSurrender : CanSurrender = fun rule cards ->
    match cards.Quantity with
    | 2<cardQty> ->
        match rule, cards.Splits with
        | SurrenderNotAllowed, _ -> SurrenderForbidden
        | SurrenderAfterSplit, _ -> SurrenderAllowed
        | SurrenderFirstHandOnly, 0<splits> -> SurrenderAllowed
        | SurrenderFirstHandOnly, _ -> SurrenderForbidden
    | _ -> SurrenderForbidden

let trySurrender hId r =
    r.PlayerHands
    |> tryFindActive hId
    |> Option.bind (fun (hId, h) -> 
        match canSurrender r.Rules.SurrenderRule h.Cards with
        | SurrenderForbidden -> None
        | SurrenderAllowed ->
            let player = r.Players |> Map.find hId.Seat
            match 
                r 
                |> decisionContext hId.Seat h
                |> player.SurrenderStrategy with
            | KeepPlaying -> None
            | SurrenderHand -> 
                let hand = h |> surrender |> Stopped
                let hands = r.PlayerHands |> Map.add hId hand
                let r = { r with PlayerHands = hands }
                Some r)
    |> Option.defaultValue r

let canDouble : CanDouble = fun rules cards ->
    match cards.Quantity with
    | 2<cardQty> -> 
        match cards.Splits, rules.MustStandOnSplitAces, cards with
        | 0<splits>, _, _ -> DoubleDownAllowed
        | _, true, FirstDealt Ace -> DoubleDownForbidden
        | _, _, _ -> DoubleDownAllowed
    | _ -> DoubleDownForbidden

let tryDouble hId r =
    r.PlayerHands
    |> tryFindActive hId
    |> Option.bind (fun (hId, h) -> 
        match canDouble r.Rules.SplitRules h.Cards with
        | DoubleDownForbidden -> None
        | DoubleDownAllowed ->
            let player = r.Players |> Map.find hId.Seat
            match 
                r 
                |> decisionContext hId.Seat h
                |> player.DoubleDownStrategy with
            | DoNotDoubleDown -> None
            | DoubleDownHand -> 
                let (card, shoe, counts) = r.Shoe |> dealFaceUp r.Counts
                let hand = h |> doubleDown card |> Stopped
                let hands = r.PlayerHands |> Map.add hId hand
                let r = 
                    { r with 
                        PlayerHands = hands 
                        Shoe = shoe
                        Counts = counts }
                Some r)
    |> Option.defaultValue r

let canSplit : CanSplit = fun rules cards ->
    match cards with 
    | PairOfCard c ->
        match rules.MaximumSplits, cards.Splits with
        | Some 0<splits>, _ -> SplitForbidden
        | Some max, current when current >= max -> SplitForbidden
        | _, _ ->
            if rules.MustStandOnSplitAces && cards.Splits >= 1<splits> && c.IsAce
            then SplitForbidden
            else SplitAllowed
    | _ -> SplitForbidden

let trySplit hId r =
    r.PlayerHands
    |> tryFindActive hId
    |> Option.bind (fun (hId, h) -> 
        match canSplit r.Rules.SplitRules h.Cards with
        | SplitForbidden -> None
        | SplitAllowed ->
            let player = r.Players |> Map.find hId.Seat
            match 
                r 
                |> decisionContext hId.Seat h
                |> player.SplitStrategy with
            | DoNotSplitHand -> None
            | SplitHand -> 
                let (card1, shoe, counts) = r.Shoe |> dealFaceUp r.Counts
                let (card2, shoe, counts) = shoe |> dealFaceUp counts
                let (h1, h2) = h |> split card1 card2
                let h2Id = r.PlayerHands |> nextHandId hId.Seat
                let hands = 
                    r.PlayerHands 
                    |> Map.add hId (Active h1)
                    |> Map.add h2Id (Active h2)
                let r = 
                    { r with 
                        PlayerHands = hands 
                        Shoe = shoe
                        Counts = counts }
                Some r)
    |> Option.defaultValue r

let canHit : CanHit = fun rules cards ->
    match cards with
    | Bust -> HitForbidden
    | _ -> 
        if rules.MustStandOnSplitAces && cards.Splits >= 1<splits> && (cards |> Cards.firstDealt).IsAce
        then HitForbidden
        else HitAllowed

let rec tryHitUntilStandOrBust hId r =
    let forceStand hId h r =
        let hand = h |> stand |> ActiveOrStoppedHand.Stopped
        let hands = r.PlayerHands |> Map.add hId hand
        { r with PlayerHands = hands }
    r.PlayerHands
    |> tryFindActive hId
    |> Option.bind (fun (hId, h) -> 
        match canHit r.Rules.SplitRules h.Cards with
        | HitForbidden -> r |> forceStand hId h |> Some
        | HitAllowed ->
            let player = r.Players |> Map.find hId.Seat
            match 
                r 
                |> decisionContext hId.Seat h
                |> player.HitStrategy with
            | Stand -> 
                r |> forceStand hId h |> Some
            | HitHand -> 
                let (card, shoe, counts) = r.Shoe |> dealFaceUp r.Counts
                let hand = h |> hit card
                let hands = r.PlayerHands |> Map.add hId hand
                let r = 
                    { r with 
                        PlayerHands = hands 
                        Shoe = shoe
                        Counts = counts }
                r
                |> tryHitUntilStandOrBust hId
                |> Some)
    |> Option.defaultValue r

let playOneHand hId r =
    r
    |> trySurrender hId
    |> trySplit hId
    |> tryDouble hId
    |> tryHitUntilStandOrBust hId

let mapPlayerHands mapping r =
    { Round.PlayerHands = r.PlayerHands |> Map.mapValues mapping
      Counts = r.Counts
      Dealer = r.Dealer
      Players = r.Players
      Rules = r.Rules
      Shoe = r.Shoe }

let playRemainingActiveHands r =
    let rec play r =
        r.PlayerHands
        |> activeHands
        |> Seq.sortBy (fun (hId, _) -> (hId.Seat, hId.Index))
        |> Seq.tryHead
        |> Option.bind (fun (hId, _) -> 
            r 
            |> playOneHand hId
            |> play
            |> Some)
        |> Option.defaultValue r
    r
    |> play
    |> mapPlayerHands (fun h -> h |> asStopped |> Option.get)

let shouldHitDealer rule cards =
    match cards, rule with
    | Hard h, _ -> h <= 16
    | Soft s, StandSoft17 -> s <= 16
    | Soft s, HitSoft17 -> s <= 17

let rec hitDealerUntilStandOrBust counts shoe rule cards =
    match cards |> shouldHitDealer rule with
    | true -> 
        let (card, shoe, count) = dealFaceUp counts shoe
        let cards = cards |> Cards.add card
        hitDealerUntilStandOrBust counts shoe rule cards
    | false -> (cards, shoe, counts)

let revealDealerHoleCard round =
    let counts = round.Counts |> Counts.countCard round.Dealer.Hole
    let dealerCards = Cards.create round.Dealer.Hole round.Dealer.FaceUp
    { Round.PlayerHands = round.PlayerHands
      Counts = counts
      Dealer = dealerCards
      Shoe = round.Shoe
      Rules = round.Rules
      Players = round.Players }

let isAlive (h:StoppedHand) =
    let isBusted = (h.Cards |> Cards.busted) 
    let isSurrendered = h.BetAdjustment = Some BetAdjustment.Surrendered
    (not isBusted) && (not isSurrendered)

let playDealerHand r = 
    match
        r.PlayerHands
        |> Map.exists (fun _ h -> h |> isAlive) with
    | false -> r
    | true -> 
        let (cards, shoe, counts) = 
            r.Dealer 
            |> hitDealerUntilStandOrBust r.Counts r.Shoe r.Rules.DealerHitRule 
        { r with 
            Dealer = cards
            Shoe = shoe 
            Counts = counts }

type ResolveBet = Cards -> StoppedHand -> CompletedHand
let resolveBet : ResolveBet = fun dealer player  ->
    let win = 1m<factor>
    let winBlackjack = 3m<factor>/2m
    let lose = -1m<factor>
    let push = 0m<factor>
    let surrendered = -1m<factor>/2m
    let insuranceWin = 2m<factor>
    let result =
        match player.Cards, dealer with
        | Blackjack, Blackjack -> push
        | Blackjack, _ -> winBlackjack
        | _, Blackjack -> lose
        | Bust, _ -> lose
        | _, Bust -> win
        | Score p, Score d -> 
            if p > d then win
            elif p < d then lose
            else push
    let doubled = 
        player.BetAdjustment 
        |> Option.bind (fun b -> 
            match b with 
            | DoubledDown -> Some player.OriginalBet
            | _ -> None)
    let total = player.OriginalBet + (doubled |> Option.defaultValue 0m<chips>)
    let profit = 
        player.BetAdjustment
        |> Option.bind (fun b ->
            match b with 
            | Surrendered -> Some (total * surrendered / 1m<factor>)
            | DoubledDown -> None)
        |> Option.defaultWith (fun () -> total * result / 1m<factor>)
    let mainBet = 
        { Bet.Wagered = total
          Profit = profit }
    let insurance = 
        player.Insurance 
        |> Option.map (fun ins -> 
            { Wagered = ins 
              Profit = 
                match dealer with
                | Blackjack -> ins * insuranceWin / 1m<factor>
                | _ -> ins * lose / 1m<factor> })
    { CompletedHand.Bet = mainBet
      Adjustment = player.BetAdjustment
      Insurance = insurance
      Cards = player.Cards }

let resolveBets r =
    r
    |> mapPlayerHands (resolveBet r.Dealer)
