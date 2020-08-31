module ConstrainedType

type Validator<'Field, 'Value, 'Error> = 'Field -> 'Value -> Result<'Value,'Error>

type ValidationError<'Value,'Rule> =
    { Field : string
      Value : 'Value
      Rule : 'Rule }

type NumberRule<'T when 'T : comparison> =
    | GreaterThanOrEqualTo of 'T
    | LessThanOrEqualTo of 'T
    | GreaterThan of 'T
    | LessThan of 'T

type StringRule =
    | NotShorterThan of int
    | NotLongerThan of int
    | NotNullOrWhitespace

type SetRule<'T when 'T: comparison> =
    | MemberOf of Set<'T>
    | NotMemberOf of Set<'T>

let validator rule param predicate =
    let validator : Validator<_,_,_> = fun field value ->
        match predicate param value with
        | true -> Ok value 
        | false -> Error { Field = field; Rule = rule param; Value = value }
    validator

let parameterlessValidator rule predicate =
    let validator : Validator<_,_,_> = fun field value ->
        match predicate value with
        | true -> Ok value 
        | false -> Error { Field = field; Rule = rule; Value = value }
    validator

let greaterThanOrEqualTo minInclusive = validator GreaterThanOrEqualTo minInclusive (fun p i -> i >= p) 
let lessThanOrEqualTo maxInclusive = validator LessThanOrEqualTo maxInclusive (fun p i -> i <= p)
let greaterThan min = validator GreaterThan min (fun p i -> i > p)
let lessThan max = validator LessThan max (fun p i -> i < p)

let memberOf set = validator MemberOf set (fun p i -> p |> Set.contains i)
let notMemberOf set = validator NotMemberOf set (fun p i -> p |> Set.contains i |> not)

let notLongerThan max = validator NotLongerThan max (fun p i -> (String.length i) <= p)
let notShorterThan min = validator NotShorterThan min (fun p i -> (String.length i) >= p)
let notNullOrWhitespace = parameterlessValidator NotNullOrWhitespace (fun s -> s |> String.isNotNullOrWhitespace)