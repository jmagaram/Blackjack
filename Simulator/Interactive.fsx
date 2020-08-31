
#I @"C:\Users\justi\source\repos\Blackjack\Blackjack\Simulator"
#load "String.fs" "ConstrainedType.fs" "Types.fs" "Measured.fs" "Array.fs" "Map.fs" "Bet.fs" "Result.fs" "Range.fs" "Card.fs" "Cards.fs" "ShoeComposition.fs" "RandomSeed.fs" "Counts.fs" "Shoe.fs" "SecretDealerHand.fs" "ActiveRound.fs" "Strategies.fs" "Simulation.fs" "Formatters.fs"

module ShoeSpeed =
    let veryLastCard shoeCount shuffler = 
        let cards =
            seq {
                for i in 1..shoeCount do
                    let shoe = shuffler()
                    yield! 
                        shoe 
                        |> Shoe.deal 
                        |> Seq.map fst
            }
        let lastCard = cards |> Seq.last
        Core.Printf.sprintf "The last card is is %A" lastCard

    let createShuffler = 
        let composition = ShoeComposition.standardShoe 8<deck>
        let depth = 75.0<pct>
        let seed = RandomSeed.timeDependent
        let shuffler = Shoe.shuffler seed depth composition
        shuffler

    let runTest () =
        let s = createShuffler
        veryLastCard 100_000 s

let zero = Bet.zero
let winEven = Bet.create 2.0m<chips> 2.0m<chips>    
let loseEven = Bet.create 2.0m<chips> -2.0m<chips>    
let push = Bet.create 1.0m<chips> 0m<chips>     
let winBlackjack = Bet.create 1.0m<chips> 1.5m<chips>
let loseHalf = Bet.create 2.0m<chips> -1.0m<chips> 
let bets = [ zero; winEven; loseEven; push; winBlackjack; loseHalf]

bets |> Seq.iter (fun b -> b |> Formatters.bet |> printfn "%s")

