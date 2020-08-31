namespace SimulatorTest 

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type ShoeCompositionTests() =

    [<DataTestMethod>]
    [<DataRow(1<deck>)>]
    [<DataRow(2<deck>)>]
    member this.StandardShoe_HasProperNumberOfEachCardType(decks:int<deck>) =
        let tensPerStandardDeck = 16<cardQty/deck>
        let nonTenPerStandardDeck = 4<cardQty/deck>
        let map = 
            ShoeComposition.standardShoe decks 
            |> ShoeComposition.itemizeCards 
            |> Seq.countBy id 
            |> Seq.map (fun (i, j) -> (i, j * 1<cardQty>))
            |> Map.ofSeq

        Assert.AreEqual(decks * tensPerStandardDeck, map |> Map.find Card.tenJackQueenOrKing)

        [Card.nine; Card.eight;Card.seven;Card.six;Card.five;Card.four;Card.three;Card.two]
        |> List.iter (fun c -> Assert.AreEqual(decks * nonTenPerStandardDeck, map |> Map.find c))

    [<DataTestMethod>]
    [<DataRow(1<deck>)>]
    [<DataRow(2<deck>)>]
    member this.StandardShoe_ItemizeListsProperNumberOfTotalCards(decks:int<deck>) =
        let composition = ShoeComposition.standardShoe decks
        let expected = decks * 52<cardQty/deck>
        let actual = composition |> ShoeComposition.itemizeCards |> Seq.length |> (*) 1<cardQty>
        Assert.AreEqual(expected, actual)