module Bet

let create wager profit =
    if wager < 0m<chips> then failwith "A wager can not be negative."
    elif wager = 0m<chips> && profit<>0m<chips> then failwith "It is not possible to win or lose when nothing is wagered."
    else
        { Bet.Wagered = wager
          Profit = profit }

let zero = create 0m<chips> 0m<chips>

let gain (bet:Bet) = (bet.Profit / bet.Wagered) * 1m<pct>

module Patterns =
    
    let (|Zero|Push|WinEven|LoseEven|WinMultiple|LoseMultiple|) (b:Bet) = 
        if b.Wagered = 0m<chips>
        then Zero
        elif b.Profit = 0m<chips>
        then Push
        elif b.Profit > 0m<chips> then
            if b.Profit = b.Wagered
            then WinEven
            else WinMultiple
        elif b.Profit = -b.Wagered
        then LoseEven
        else LoseMultiple
