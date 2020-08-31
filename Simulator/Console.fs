module Console

open System

let yesOrNo prompt =
    printf "%s (Y/N) " prompt
    let response = Console.ReadKey()
    printfn ""
    response.Key = ConsoleKey.Y
