module Map

let mapValues mapping map =
    map
    |> Map.map (fun _ value -> mapping value)

let keys map = 
    map 
    |> Map.toSeq
    |> Seq.map (fun (key, value) -> key)