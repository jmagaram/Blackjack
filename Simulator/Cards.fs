module Cards

let create c1 c2 = 
    { Cards.Cards = [ c2; c1 ] 
      HasAce = c1.IsAce || c2.IsAce
      MinTotal = (c1.MinValue + c2.MinValue) * 1<score>
      IsSplittable = c1 = c2
      Quantity = 2<cardQty>
      Splits = 0<splits> }

let add c (cards:Cards) = 
    { Cards.Cards = c :: cards.Cards
      HasAce = cards.HasAce || c.IsAce
      MinTotal = cards.MinTotal + c.MinValue * 1<score>
      IsSplittable = false
      Quantity = cards.Quantity + 1<cardQty> 
      Splits = cards.Splits }

let splittable cards = 
    match cards.IsSplittable with
    | true -> Some cards.Cards.Head
    | false -> None

let splittableValue cards = 
    match cards |> splittable with
    | Some (Card.Patterns.MinCardValue c) -> Some c
    | None -> None

let split cardA cardB cards =
    match cards |> splittable with
    | Some c -> 
        let splitCount = cards.Splits + 1<splits>
        let create card1 card2 splits = { create card1 card2 with Splits = splits }
        (create c cardA splitCount, create c cardB splitCount)
    | None -> failwith "Can't split those cards."

let score cards =
    if cards.MinTotal <= 11<score> && cards.HasAce
    then cards.MinTotal + 10<score>
    else cards.MinTotal

let busted cards = (score cards) > 21<score>

let firstDealt (cards:Cards) = cards.Cards |> Seq.last

let isBlackjack cards = 
    (cards.MinTotal = 11<score>) 
    && (cards.HasAce) 
    && (cards.Quantity = 2<cardQty>)
    && (cards.Splits = 0<splits>)

module Patterns = 

    let (|PairOfCard|_|) (cards:Cards) = 
        cards 
        |> splittable 
        |> Option.map (fun c -> PairOfCard c)

    let (|PairOf|_|) (cards:Cards) =
        cards 
        |> splittableValue
        |> Option.map (fun v -> PairOf v)

    let (|Blackjack|Bust|Score|) (cards:Cards) =
        if (isBlackjack cards) 
        then Blackjack
        else
            let s = score cards
            if s > 21<score> 
            then Bust
            else Score s

    let (|Hard|Soft|) (cards:Cards) =
        if cards.MinTotal > 11<score> // weird case 18 aces is not soft
        then Hard (cards.MinTotal / 1<score>)
        else
            if cards.HasAce
            then Soft ((cards.MinTotal + 10<score>) / 1<score>)
            else Hard (cards.MinTotal / 1<score>)

    let (|FirstDealt|) (cards:Cards) = cards |> firstDealt

