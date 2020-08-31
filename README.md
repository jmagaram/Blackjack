# Blackjack Simulator

I was inspired by [Enterprise Tic Tac Toe](https://fsharpforfunandprofit.com/posts/enterprise-tic-tac-toe/) and decided to build this blackjack simulator. The simulator allows you to programmatically define various blackjack card-counting strategies and simulate thousands of hands to see how well the strategies perform. The point of this exercise was NOT to find the best blackjack strategy; I really just wanted to find out how easy it is to build this kind of thing with functional programming.

Although this code does a lot of complex things, there were relatively few bugs along the way. I believe much of the code is easy-to-understand. I was very surprised to find out that the entire simulator worked almost immediately after it compiled. Other people have said that with functional programming that if it compiles it probably works, and that seemed to be true for me.

If you actually want to use this to simulate a strategy, you'll need to write some F# code, like that found in the `BasicStrategy` module, to define a player (see below), and then plug that into the `main` in `Program.fs`

    type Player = 
        { PlayerName : string
          CountingSystem : CountingSystem option
          BuyInStrategy : BuyInStrategy
          InsuranceStrategy : InsuranceStrategy
          SurrenderStrategy : SurrenderStrategy
          SplitStrategy : SplitStrategy
          DoubleDownStrategy : DoubleDownStrategy
          HitStrategy : HitStrategy }

Each part of a player's strategy is simply a function that takes the current situation, like cards in hand, the dealer's face up card, and the count, and decides (calculates) what to do. For example, here is how the `SplitStrategy` is defined...

    type SplitStrategy = (DecisionContext -> SplitDecision)

    and SplitDecision =
        | SplitHand
        | DoNotSplitHand

    type DecisionContext =
        { PlayerCards : Cards
          DealerFaceUpCard : Card
          Count : CardCount option }

