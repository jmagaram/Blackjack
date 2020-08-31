[<AutoOpen>]
module Measured

let toMeasuredInt (value:float<'measure>) = LanguagePrimitives.Int32WithMeasure<'measure>(int value)

let toMeasuredFloat (value:int<'measure>) = LanguagePrimitives.FloatWithMeasure<'measure>(float value)

let toMeasuredDecimal (value:int<'measure>) = LanguagePrimitives.DecimalWithMeasure<'measure>(decimal value)
