module RandomSeed

let timeDependent = TimeDependent

let seedValue i = RandomSeed i

let toRandom seed =
    match seed with
    | TimeDependent -> new System.Random()
    | RandomSeed s -> new System.Random(s)