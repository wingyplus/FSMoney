﻿namespace FSMoney

module Money =
    // TODO: Add support for divide.

    type Money = { Amount: int; Currency: string }

    type Cmp =
        | Lt
        | Eq
        | Gt

    let internal failCurrenciesMustEqual a b =
        invalidArg "Money.Currency" (sprintf "Currency %s must be the same as %s" a.Currency b.Currency)

    let create amount currency = { Amount = amount; Currency = currency }

    let isZero { Amount = amount } = amount = 0

    let isPositive { Amount = amount } = amount > 0

    let isNegative { Amount = amount } = amount < 0

    let equals { Amount = a1; Currency = c1 } { Amount = a2; Currency = c2 } = a1 = a2 && c1 = c2

    let compare a b =
        match (a.Currency, b.Currency) with
        | (c1, c2) when c1 = c2 ->
            match a.Amount - b.Amount with
            | x when x < 0 -> -1
            | x when x = 0 -> 0
            | _ (* when x > 0 *)  -> 1
        | _ -> failCurrenciesMustEqual a b

    let cmp a b =
        match compare a b with
        | -1 -> Lt
        | 0 -> Eq
        | _ (* 1 *)  -> Gt

    let addFloat m f =
        { Amount = m.Amount + ((f * 100.0) |> round |> int)
          Currency = m.Currency }

    let addInt m n =
        { Amount = m.Amount + n
          Currency = m.Currency }

    let add m b =
        match box b with
        | :? float as f -> addFloat m f
        | :? int as n -> addInt m n
        | _ -> invalidArg "b" "b must be int or float"

    let subtractFloat m f =
        { Amount = m.Amount - ((f * 100.0) |> round |> int)
          Currency = m.Currency }

    let subtractInt m n =
        { Amount = m.Amount - n
          Currency = m.Currency }

    let subtract m b =
        match box b with
        | :? float as f -> subtractFloat m f
        | :? int as n -> subtractInt m n
        | _ -> invalidArg "b" "b must be int or float"

    let multiplyInt m n =
        { Amount = m.Amount * n
          Currency = m.Currency }

    let multiplyFloat m f =
        { Amount = (float m.Amount) * f |> round |> int
          Currency = m.Currency }

    let multiply m b =
        match box b with
        | :? float as f -> multiplyFloat m f
        | :? int as n -> multiplyInt m n
        | _ -> invalidArg "b" "b must be int or float"