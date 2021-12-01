let input =
    [| 100
       101
       105
       106
       103
       104
       106
       108
       112
       123
       125
       149
       158
       147
       150
       153
       156
       166
       171
       172
       174
       186
       193
       198
       203
       217 |]

let countRisingQuantities (i:int[]) =
    i
    |> Array.pairwise
    |> Array.filter (fun (p1, p2) -> p2 > p1)
    |> Array.length

printfn "%i" (countRisingQuantities input)

let countRisingQuantitiesTriples (i:int[]) =
    i
    |> Array.windowed 3
    |> Array.map Array.sum
    |> Array.pairwise
    |> Array.filter (fun (p1, p2) -> p2 > p1)
    |> Array.length

printfn "%i" (countRisingQuantitiesTriples input)
