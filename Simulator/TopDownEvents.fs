module TopDownEvents

type PlayerDecision = 
    | BuyIn of PlayerId * Chips * CardStatistics
    | PurchaseInsurance of HandId * Chips 
    | Surrender of HandId
    | Split of HandId
    | DoubleDown of HandId * Chips
    | Stand of HandId
    | Hit of HandId

and CardStatistics = int

and DealerAction = 
    | ShuffleShoe
    | DealInitialCards
    | RevealBlackjack
    | CollectInsuranceBets
    | CollectCards

and PlayerId = int
and HandId = int
and Chips = int

// if ACE up - offer insurance and record the purchases
// if TEN up - offer insurance and record the purchases
// if NEITHER 

// when did I decline insurance vs. not available?
// when did I decline double down?

