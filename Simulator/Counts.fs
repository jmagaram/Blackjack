module Counts

let initialize players =
    players
    |> Map.toSeq
    |> Seq.choose (fun (s, p) -> p.CountingSystem |> Option.map (fun sys -> (s, (sys.Start, sys))))
    |> Map.ofSeq        
    |> Counts

let countCard card (Counts c) =
    c 
    |> Map.mapValues (fun (cnt, sys) -> (sys.CountCard card cnt, sys))
    |> Counts

let newShoe (Counts c) =
    c 
    |> Map.mapValues (fun (_, sys) -> (sys.Start, sys))
    |> Counts

let forSeat seat (Counts c) =
    c
    |> Map.tryFind seat
    |> Option.map fst