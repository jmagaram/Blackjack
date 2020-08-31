module ShoeComposition

open Card.Patterns

let standardShoe (decks:int<deck>) =
    let suits = 4
    let quantity card =
        match card with
        | Ten -> decks / 1<deck> * 4<cardQty> * suits
        | _ -> decks / 1<deck> * 1<cardQty> * suits
    [ Card.ace; Card.two; Card.three; Card.four; Card.five; Card.six; Card.seven; Card.eight; Card.nine; Card.tenJackQueenOrKing ]
    |> Seq.map (fun card -> card, quantity card)
    |> Map.ofSeq
    |> ShoeComposition

let itemizeCards (ShoeComposition composition) = 
    composition
    |> Map.toSeq
    |> Seq.map (fun (card, quantity) -> card |> Seq.replicate (quantity / 1<cardQty>))
    |> Seq.concat
