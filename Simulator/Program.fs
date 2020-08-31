open System
open System.Text
open Strategies

module CompletedRounds = 

    let private betsPerSeat (r:CompletedRound) =
        r.CompletedHands
        |> Map.toSeq
        |> Seq.map (fun (hid, h) -> (hid.Seat, h.TotalBet))

    let summarizeBySeat (r: CompletedRound seq) =
        r
        |> Seq.collect betsPerSeat
        |> Seq.fold (fun total (seat, bet) -> 
            total 
            |> Map.tryFind seat
            |> Option.map (fun previous -> previous + bet)
            |> Option.defaultValue Bet.zero
            |> function current -> total |> Map.add seat current) Map.empty

let createBasicStrategyPlayer name = BasicStrategy.create name

let smarterHighLowPlayer (rules:Rules) playerName = 
    let totalCards = 
        rules.ShoeComposition 
        |> ShoeComposition.itemizeCards 
        |> Seq.length 
        |> (*) 1<cardQty>
    let highLow = Strategies.HighLow.create totalCards
    let insurance : InsuranceStrategy = fun ctx ->
        match ctx.Count with
        | None -> InsuranceDecision.DeclineInsurance
        | Some c -> 
            let trueCount = (highLow.GetStatistics c).TrueCount()
            if (trueCount <= 2m) then DeclineInsurance
            else PurchaseInsurance
    let betting : BuyInStrategy = fun count ->
        match count with
        | None -> Some 1m<factor>
        | Some c -> 
            let trueCount = (highLow.GetStatistics c).TrueCount()
            if (trueCount < 1m) then Some 1m<factor>
            else Some ((trueCount + 1m) * 5m<factor>)
    { createBasicStrategyPlayer playerName with 
        Player.PlayerName = playerName
        Player.CountingSystem = Some highLow.System
        Player.BuyInStrategy = betting 
        Player.InsuranceStrategy = insurance }

[<EntryPoint>]
let main argv =
    printfn "Simulating blackjack"
    printfn ""
    let rules = 
        { Rules.BetRange = Range.create 1m<chips> 100m<chips>
          Rules.SurrenderRule = SurrenderFirstHandOnly
          Rules.DealDepth = 80.0<pct>
          Rules.SplitRules = { 
            SplitRules.MaximumSplits = None 
            SplitRules.MustStandOnSplitAces = true }
          Rules.ShoeComposition = ShoeComposition.standardShoe 8<deck>
          Rules.DealerHitRule = DealerHitRule.HitSoft17 }
    let seed = RandomSeed.timeDependent
    let justin = createBasicStrategyPlayer "Justin (Basic Strategy)"
    let ezra = smarterHighLowPlayer rules "Ezra (HiLo Strategy)"
    let players =
        Map.empty
        |> Map.add 0<seat> justin
        |> Map.add 1<seat> ezra
    let rounds = 100_000
    Console.WriteLine(sprintf "Beginning simulation of %i hands." rounds)
    let maybeDisplayEachRound =
        match Console.yesOrNo "Display each hand?" with
        | false -> 
            fun (r:CompletedRound) -> r
        | true -> 
            fun (r:CompletedRound) -> 
            Console.Write(Formatters.completedRound players r)
            Console.ReadLine() |> ignore
            r
    let totalBets = 
        Simulation.start rules seed players
        |> Seq.take rounds
        |> Seq.map maybeDisplayEachRound
        |> CompletedRounds.summarizeBySeat
        |> Map.toSeq
        |> Seq.map (fun (s,b) -> ((players |> Map.find s).PlayerName),b)
        |> Map.ofSeq
    printfn "Rounds   %i" rounds
    totalBets 
        |> Seq.iter (fun i -> 
            let name = i.Key.PadLeft(15)
            let gain = i.Value |> Formatters.betGain
            printfn "%s win/lose: %s" name gain)

    0 // return an integer exit code

