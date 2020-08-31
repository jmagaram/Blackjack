namespace SimulatorTest 

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type ShoeTests () =

    [<DataTestMethod>]
    [<DataRow(1,52)>]
    [<DataRow(2,104)>]
    member this.DealUntilEmpty_ReturnsProperNumberOfCards(decks:int, expectedCards:int) =
        let seed = RandomSeed.TimeDependent
        let dealDepth = 50.0<pct>
        let decks = decks * 1<deck>
        let composition = ShoeComposition.standardShoe decks
        let shuffler = Shoe.shuffler seed dealDepth composition
        let shoe = shuffler()
        let totalCards = shoe |> Shoe.deal |> Seq.length
        Assert.AreEqual(expectedCards, totalCards)

    [<DataTestMethod>]
    [<DataRow(0.0<pct>,0<cardQty>)>]
    [<DataRow(50.0<pct>,26<cardQty>)>]
    [<DataRow(10.0<pct>,5<cardQty>)>]
    [<DataRow(100.0<pct>,52<cardQty>)>]
    member this.IsShuffleRequired(percent:float<pct>,cardsUntilShuffleRequired:int<cardQty>) =
        let seed = RandomSeed.TimeDependent
        let dealDepth = percent
        let decks = 1<deck>
        let composition = ShoeComposition.standardShoe decks
        let shuffler = Shoe.shuffler seed dealDepth composition
        let shoe = shuffler()
        let actual =
            let generator shoe =
                match shoe with
                | Shoe.Patterns.ShuffleRequired -> None
                | Shoe.Patterns.CutCardNotYetReached -> shoe |> Shoe.deal |> Seq.head |> Some
            Seq.unfold generator shoe
            |> Seq.length
        Assert.AreEqual(cardsUntilShuffleRequired, actual)