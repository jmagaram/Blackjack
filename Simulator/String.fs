module String

let isNullOrWhitespace (s:string) = System.String.IsNullOrWhiteSpace(s)

let isNotNullOrWhitespace (s:string) = not (System.String.IsNullOrWhiteSpace(s))
    
let trim (s:string) = s.Trim()
   