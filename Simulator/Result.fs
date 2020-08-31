module Result

let inline successOrThrow result =
    match result with
    | Error error -> failwith ((error :> System.Object).ToString())
    | Ok success -> success

let inline toSuccessOption result =
    match result with
    | Error error -> None
    | Ok success -> Some success
