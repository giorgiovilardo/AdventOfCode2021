module Input =
    let private readLines (filePath: string) =
        Seq.toList (
            seq {
                use sr = new System.IO.StreamReader(filePath)

                while not sr.EndOfStream do
                    yield sr.ReadLine()
            }
        )

    let parsed = readLines "Inputs/day3.txt"

    let basic =
        Seq.toList (
            seq {
                "00100"
                "11110"
                "10110"
                "10111"
                "10101"
                "01111"
                "00111"
                "11100"
                "10000"
                "11001"
                "00010"
                "01010"
            }
        )

module DayThree =
    let bitStringBy f i =
        i
        |> Seq.map (f snd)
        |> Seq.map fst
        |> Seq.map string
        |> String.concat ""

    let bitCount (s: seq<string>) =
        s
        |> Seq.map Seq.toList
        |> Seq.transpose
        |> Seq.map (Seq.countBy id)

    let gammaEpsilon bitCount =
        (bitStringBy Seq.maxBy bitCount, bitStringBy Seq.minBy bitCount)

    let part1result =
        let g, e = (bitCount >> gammaEpsilon) Input.parsed

        System.Convert.ToInt32(g, 2)
        * System.Convert.ToInt32(e, 2)

    let bitCounterAtPositionInArray defaultIfDraw countingFunction counter input =
        input
        |> List.map Seq.toList
        |> List.transpose
        |> fun lists -> lists.[counter]
        |> List.countBy id
        |> fun x ->
            match x with
            | [ (_, value0); (_, value1) ] when value1 = value0 -> defaultIfDraw
            | x -> (countingFunction snd x) |> fst

    let rec oxygenFinder counter (input: string list) =
        match input with
        | [ x ] -> [ x ]
        | xs ->
            oxygenFinder
                (counter + 1)
                (xs
                 |> List.filter (fun x -> x.[counter] = (bitCounterAtPositionInArray '1' List.maxBy counter xs)))

    let rec co2Finder counter (input: string list) =
        match input with
        | [ x ] -> [ x ]
        | xs ->
            co2Finder
                (counter + 1)
                (xs
                 |> List.filter (fun x -> x.[counter] = (bitCounterAtPositionInArray '0' List.minBy counter xs)))

    let part2result =
        let finders i =
            (List.head (oxygenFinder 0 i), List.head (co2Finder 0 i))

        let o, c = finders Input.parsed

        System.Convert.ToInt32(o, 2)
        * System.Convert.ToInt32(c, 2)
