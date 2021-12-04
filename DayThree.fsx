module Input =
    let private readLines (filePath: string) =
        seq {
            use sr = new System.IO.StreamReader(filePath)

            while not sr.EndOfStream do
                yield sr.ReadLine()
        }

    let parsed = readLines "Inputs/day3.txt"

    let basic =
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
