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
       153 |]

let countRisingQuantities =
    input
    |> Array.pairwise
    |> Array.filter (fun (p1, p2) -> p2 > p1)
    |> Array.length
