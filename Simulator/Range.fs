module Range

let create min max =
    if (min >= max)
    then failwith "The minimum must be less than the maximum."
    else { MinInclusive = min; MaxInclusive = max}

let validate<'T when 'T:comparison> i range =
    if (i>=range.MinInclusive && i<=range.MaxInclusive)
    then Some i
    else None

let min<'T> (range:Range<'T>) = range.MinInclusive

let max<'T> (range:Range<'T>) = range.MaxInclusive

let coerce i (range:Range<_>) =
    if i<range.MinInclusive then range.MinInclusive
    elif i>range.MaxInclusive then range.MaxInclusive
    else i
