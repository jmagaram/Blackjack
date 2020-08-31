namespace SimulatorTest 

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type ArrayTests () =

    let swap = Array.swap
    let shuffle = Array.shuffle

    let shuffleDistribution items iterations =
        let random =
            RandomSeed.TimeDependent 
            |> RandomSeed.toRandom
        let shuffle () = 
            let source = items |> Seq.toArray
            source |> shuffle random
            source |> Seq.fold (fun state item -> state + item) ""
        [1..iterations]
        |> Seq.map (fun _ -> shuffle())
        |> Seq.countBy (fun i -> i)
        |> Seq.toList

    member this.AssertArraysHaveEqualMembers x y =
        Assert.AreEqual(x |> Array.length, y |> Array.length)
        Assert.IsTrue(x |> Seq.zip y |> Seq.forall(fun (i,j)->i=j))

    [<TestMethod>]
    member this.Swap_WhenThreeItemsAndIndexesOnEnds_WillSwap() =
        let source = [|1;2;3|]
        swap 0 2 source
        let expected = [|3;2;1|]
        this.AssertArraysHaveEqualMembers source expected

    [<TestMethod>]
    [<DataRow(0)>]
    [<DataRow(1)>]
    [<DataRow(2)>]
    member this.Swap_WhenThreeItemsAndIndexesAreSame_WillDoNothing(index:int) =
        let source = [|1;2;3|]
        swap index index source
        let expected = [|1;2;3|]
        this.AssertArraysHaveEqualMembers source expected

    [<TestMethod>]
    member this.Swap_WhenTwoItemsAndIndexesAreDifferent_WillSwap() =
        let source = [|1;2|]
        swap 0 1 source
        let expected = [|2;1;|]
        this.AssertArraysHaveEqualMembers source expected

    [<TestMethod>]
    member this.Swap_WhenOneItem_WillDoNothing() =
        let source = [|1|]
        swap 0 0 source
        let expected = [|1|]
        this.AssertArraysHaveEqualMembers source expected

    [<TestMethod>]
    member this.Shuffle_WhenOneItem_WillDoNothing() =
        let iterations = 1000
        let items = ["A"]
        let permutations = 1
        let percentOfOptimalGoal = 100
        let goal = iterations * percentOfOptimalGoal / 100 / permutations
        let distribution = shuffleDistribution items iterations
        Assert.AreEqual(permutations, distribution |> Seq.length)
        Assert.IsTrue(distribution |> Seq.forall (fun (item,count) -> count >=goal))
    
    [<TestMethod>]
    member this.Shuffle_WhenTwoItems_ResultsEvenlyDistributed() =
        let iterations = 1000000
        let items = ["A";"B"]
        let permutations = 2
        let percentOfOptimalGoal = 98
        let goal = iterations * percentOfOptimalGoal / 100 / permutations
        let distribution = shuffleDistribution items iterations
        Assert.AreEqual(permutations, distribution |> Seq.length)
        Assert.IsTrue(distribution |> Seq.forall (fun (item,count) -> count >=goal))

    [<TestMethod>]
    member this.Shuffle_WhenThreeItems_ResultsEvenlyDistributed() =
        let iterations = 1000000
        let items = ["A";"B";"C"]
        let permutations = 6
        let percentOfOptimalGoal = 98
        let goal = iterations * percentOfOptimalGoal / 100 / permutations
        let distribution = shuffleDistribution items iterations
        Assert.AreEqual(permutations, distribution |> Seq.length)
        Assert.IsTrue(distribution |> Seq.forall (fun (item,count) -> count >=goal))
