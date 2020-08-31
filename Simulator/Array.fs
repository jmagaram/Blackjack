module Array

let swap indexA indexB items =
    let aValue = Array.get items indexA
    let bValue = Array.get items indexB
    Array.set items indexA bValue
    Array.set items indexB aValue

let shuffle (random:System.Random) items =
    let count = Array.length items
    for i in [0..count-2] do
        let j = random.Next(i,count)
        swap i j items
