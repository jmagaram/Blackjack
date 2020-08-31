module SecretDealerHand

open Card.Patterns

let isBlackjack hand =
    match hand.FaceUp, hand.Hole with
    | Ace, Ten -> true
    | Ten, Ace -> true
    | _, _ -> false
