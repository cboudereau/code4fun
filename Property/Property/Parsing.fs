module Parsing

open System
open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern, RegexOptions.CultureInvariant ||| RegexOptions.IgnoreCase)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let (|Int|_|) x = match Int32.TryParse(x) with (true, x') -> Some x' | _ -> None

let (|Currency|_|) x = 
    match Decimal.TryParse(x, Globalization.NumberStyles.Currency, Globalization.CultureInfo.InvariantCulture) with 
    | (true, x') -> Some x' 
    | _ -> None

let split (separators:string list) (x:string) = x.Split(separators |> List.toArray, StringSplitOptions.RemoveEmptyEntries)

let icontains search (source:string) = source.IndexOf(search, StringComparison.InvariantCultureIgnoreCase) <> -1
