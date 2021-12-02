let input =
    [ "forward 5"
      "down 5"
      "forward 8"
      "up 3"
      "down 8"
      "forward 2" ]

module Submarine =
    type Command =
        | Forward of int
        | Down of int
        | Up of int

    type SubmarineData = { Depth: int; Horizontal: int }
    let defaultSubmarine = { Depth = 0; Horizontal = 0 }

    let parseCommand (input: string) =
        match List.ofArray (input.Split(' ')) with
        | [ command; data ] when command = "forward" -> data |> int |> Forward
        | [ command; data ] when command = "down" -> data |> int |> Down
        | [ command; data ] when command = "up" -> data |> int |> Up
        | _ -> failwith "impossibiru"

    let updateDataFromCommand submarineData command =
        match command with
        | Forward x ->
            { submarineData with
                  Horizontal = submarineData.Horizontal + x }
        | Down x ->
            { submarineData with
                  Depth = submarineData.Depth + x }
        | Up x ->
            { submarineData with
                  Depth = submarineData.Depth - x }

let resultPart1 input =
    input
    |> List.map Submarine.parseCommand
    |> List.fold Submarine.updateDataFromCommand Submarine.defaultSubmarine
    |> fun data -> data.Depth * data.Horizontal

let resultPart2 input =
    input
    |> List.map Submarine.parseCommand
    |> List.fold
        (fun state data ->
            match data with
            | Submarine.Forward amount ->
                {| Depth = state.Depth + amount * state.Aim
                   Horizontal = state.Horizontal + amount
                   Aim = state.Aim |}
            | Submarine.Up amount ->
                {| Depth = state.Depth
                   Horizontal = state.Horizontal
                   Aim = state.Aim - amount |}
            | Submarine.Down amount ->
                {| Depth = state.Depth
                   Horizontal = state.Horizontal
                   Aim = state.Aim + amount |})
        {| Depth = 0; Horizontal = 0; Aim = 0 |}
    |> fun data -> data.Depth * data.Horizontal

printfn $"%i{resultPart1 input}"
printfn $"%i{resultPart2 input}"
