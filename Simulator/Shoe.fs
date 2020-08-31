module Shoe

let shuffler seed depth composition =
    let random = 
        seed 
        |> RandomSeed.toRandom
    let unshuffled =
        composition
        |> ShoeComposition.itemizeCards
        |> Seq.toArray
    let maxCardsInShoe = 
        unshuffled 
        |> Array.length
        |> (*) 1<cardQty>
    let untilCutCard = 
        depth / 100.0<pct> * (maxCardsInShoe |> toMeasuredFloat) |> toMeasuredInt
    fun () ->
        { Cards = 
            let cards = unshuffled |> Array.copy  
            Array.shuffle random cards
            cards
          NextCard = 0<index>
          UntilCutCard = untilCutCard }

let deal shoe =
    let unfold (s:Shoe) =
        match s.NextCard / 1<index> <= (s.Cards.Length - 1) with
        | true ->
            let c = s.Cards.[s.NextCard / 1<index>]
            let s = 
                { s with
                    UntilCutCard = s.UntilCutCard - 1<cardQty>
                    NextCard = s.NextCard + 1<index> }
            Some ((c,s),s)
        | false -> None
    Seq.unfold unfold shoe

let isShuffleRequired shoe = 
    shoe.UntilCutCard <= 0<cardQty>

module Patterns = 

    let (|ShuffleRequired|CutCardNotYetReached|) shoe =
        match shoe |> isShuffleRequired with
        | true -> ShuffleRequired
        | false -> CutCardNotYetReached
