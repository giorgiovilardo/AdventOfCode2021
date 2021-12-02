let input =
    [ "forward 5"
      "down 5"
      "forward 8"
      "up 3"
      "down 8"
      "forward 2" ]

type SubmarineCommand =
    | Forward of int
    | Down of int
    | Up of int

type SubmarineData = { Depth: int; Horizontal: int }
let createSubmarineData = { Depth = 0; Horizontal = 0 }

let parseCommand (input: string) =
    let splittedList = List.ofArray (input.Split(' '))

    match splittedList with
    | [ command; data ] when command = "forward" -> data |> int |> Forward
    | [ command; data ] when command = "down" -> data |> int |> Down
    | [ command; data ] when command = "up" -> data |> int |> Up
    | _ -> failwith "impossibiru"

let updateSubmarineDataFromCommand submarineData command =
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

let k =
    let subFinalState =
        input
        |> List.map parseCommand
        |> List.fold updateSubmarineDataFromCommand createSubmarineData

    subFinalState.Depth * subFinalState.Horizontal

printfn "%A" k
