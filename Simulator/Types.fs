[<AutoOpen>]
module Types

open System

[<Measure>] type cardQty
[<Measure>] type pct
[<Measure>] type deck
[<Measure>] type chips
[<Measure>] type factor
[<Measure>] type score
[<Measure>] type index
[<Measure>] type seat
[<Measure>] type hand
[<Measure>] type splits

[<Struct>]
[<StructuredFormatDisplay("{Compact}")>]
type Card = 
    | AceToTen of int
    member this.Compact = 
        match this with 
        | AceToTen 10 -> "T"
        | AceToTen 1 -> "A"
        | AceToTen x -> x.ToString()
    member this.MinValue = match this with AceToTen x -> x
    member this.IsAce = match this with AceToTen x -> x = 1
    member this.IsTen = match this with AceToTen x -> x = 10

type Shoe =
    { Cards : Card[]
      NextCard : int<index>
      UntilCutCard : int<cardQty> }

type Bet = 
    { Wagered : decimal<chips>
      Profit : decimal<chips> }
    static member (+) (b1 : Bet, b2 : Bet) =
        { Wagered = b1.Wagered + b2.Wagered 
          Profit = b1.Profit + b2.Profit }
    static member (+) (b1 : Bet, b2 : Bet option) = b1
    static member (+) (b1 : Bet option, b2 : Bet) = b2

type RandomSeed = 
    | TimeDependent 
    | RandomSeed of int

type ShoeComposition = ShoeComposition of Map<Card, int<cardQty>>

type CardCount = CardCount of Map<string, decimal>

type CountingSystem =
    { Start : CardCount
      CountCard : Card -> CardCount -> CardCount }

type Cards = 
    { Cards : Card list
      Splits : int<splits>
      Quantity : int<cardQty>
      HasAce : bool
      IsSplittable : bool
      MinTotal : int<score> }

type DecisionContext =
    { PlayerCards : Cards
      DealerFaceUpCard : Card
      Count : CardCount option }

type SurrenderStrategy = (DecisionContext -> SurrenderDecision)

and SurrenderDecision = 
    | SurrenderHand 
    | KeepPlaying

type InsuranceStrategy = (DecisionContext -> InsuranceDecision)

and InsuranceDecision =
    | PurchaseInsurance
    | DeclineInsurance

type SplitStrategy = (DecisionContext -> SplitDecision)

and SplitDecision =
    | SplitHand
    | DoNotSplitHand

type DoubleDownStrategy = (DecisionContext -> DoubleDownDecision)

and DoubleDownDecision =
    | DoubleDownHand
    | DoNotDoubleDown

type HitStrategy = (DecisionContext -> HitDecision)

and HitDecision =
    | HitHand
    | Stand

type BuyInStrategy = CardCount option -> decimal<factor> option

type Player = 
    { PlayerName : string
      CountingSystem : CountingSystem option
      BuyInStrategy : BuyInStrategy
      InsuranceStrategy : InsuranceStrategy
      SurrenderStrategy : SurrenderStrategy
      SplitStrategy : SplitStrategy
      DoubleDownStrategy : DoubleDownStrategy
      HitStrategy : HitStrategy }

type SecretDealerHand =
    { FaceUp : Card
      Hole : Card }

type Seat = int<seat>

type Players = Map<Seat, Player>

type HandIndex = int<hand>

type HandId = 
    { Seat : Seat
      Index : HandIndex }

type BetAdjustment = 
    | Surrendered
    | DoubledDown

type CompletedHand =
    { Cards : Cards
      Bet : Bet
      Adjustment : BetAdjustment option
      Insurance : Bet option }
    member this.TotalBet = this.Bet + this.Insurance

type CompletedRound = 
    { CompletedHands : Map<HandId, CompletedHand>
      Dealer : Cards }

type SplitRules =
    { MaximumSplits : int<splits> option  
      MustStandOnSplitAces : bool }

type SurrenderRule =
    | SurrenderNotAllowed
    | SurrenderFirstHandOnly
    | SurrenderAfterSplit

type DealerHitRule =
    | StandSoft17
    | HitSoft17

type Range<'T> = 
    { MinInclusive : 'T; 
      MaxInclusive : 'T }

type Rules =
    { BetRange : Range<decimal<chips>>
      DealDepth : float<pct>
      SurrenderRule : SurrenderRule
      SplitRules : SplitRules
      DealerHitRule : DealerHitRule
      ShoeComposition: ShoeComposition }

type Simulation = Rules -> RandomSeed -> Players -> CompletedRound seq

// Implementation below

type Counts = Counts of Map<Seat, CardCount * CountingSystem>

type StoppedHand = 
    { Cards : Cards
      Insurance : decimal<chips> option
      OriginalBet : decimal<chips> 
      BetAdjustment : BetAdjustment option }

type ActiveHand = 
    { Cards : Cards
      Insurance : decimal<chips> option
      OriginalBet : decimal<chips> }

type ActiveOrStoppedHand =
    | Active of ActiveHand
    | Stopped of StoppedHand 

type PurchaseInsurance = ActiveHand -> ActiveHand

type LoseInsuranceIfAny = ActiveHand -> StoppedHand

type Surrender = ActiveHand -> StoppedHand

type Split = Card -> Card -> ActiveHand -> ActiveHand * ActiveHand

type Hit = Card -> ActiveHand -> ActiveOrStoppedHand

type Stand = ActiveHand -> StoppedHand

type DoubleDown = Card -> ActiveHand -> StoppedHand

type CanSurrender = SurrenderRule -> Cards -> CanSurrenderResult

and CanSurrenderResult =
    | SurrenderAllowed
    | SurrenderForbidden

type CanSplit = SplitRules -> Cards -> CanSplitResult

and CanSplitResult =
    | SplitAllowed
    | SplitForbidden

type CanDouble = SplitRules -> Cards -> CanDoubleResult

and CanDoubleResult =
    | DoubleDownAllowed
    | DoubleDownForbidden

type CanHit = SplitRules -> Cards -> CanHitResult

and CanHitResult =
    | HitAllowed
    | HitForbidden

type Round<'PlayerHand,'Dealer> =
    { PlayerHands : Map<HandId, 'PlayerHand>
      Counts : Counts
      Shoe : Shoe
      Players : Players
      Rules : Rules
      Dealer : 'Dealer } 
