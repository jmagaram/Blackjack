module Card

let createUnsafe field i =
    i
    |> Ok
    |> Result.bind (ConstrainedType.greaterThanOrEqualTo 1 field)
    |> Result.bind (ConstrainedType.lessThanOrEqualTo 10 field)
    |> Result.map AceToTen
    |> Result.successOrThrow

let fieldName = "singleton"
let ace = createUnsafe fieldName 1
let two = createUnsafe fieldName 2
let three = createUnsafe fieldName 3
let four = createUnsafe fieldName 4
let five = createUnsafe fieldName 5
let six = createUnsafe fieldName 6
let seven = createUnsafe fieldName 7
let eight = createUnsafe fieldName 8
let nine = createUnsafe fieldName 9
let tenJackQueenOrKing = createUnsafe fieldName 10

let minValue (AceToTen cardValue) = cardValue

module Patterns =

    let (|Ace|Ten|TwoThroughNine|) (AceToTen cardValue) =
        match cardValue with
        | 10 -> Ten
        | 1 -> Ace
        | x -> TwoThroughNine x

    let (|MinCardValue|) (AceToTen cardValue) = MinCardValue cardValue

