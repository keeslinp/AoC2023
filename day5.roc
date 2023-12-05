app "AoC"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.2.0/dJQSsSmorujhiPNIvJKlQoI92RFIG_JQwUfIxZsCSwE.tar.br",
    }
    imports [pf.Stdout, pf.Task.{ Task }, "day-5-input.txt" as input : Str, parser.Core.{ Parser, keep, skip, oneOrMore,  string, const,  sepBy,  chompUntil, many }, parser.String.{ parseStr, digits, string, codeunit }]
    provides [main] to pf

main : Task {} *
main =
    _ <- input |> part1 |> Num.toStr |> Stdout.line |> Task.await
    input |> part2 |> Num.toStr |> Stdout.line

# skipWhiteSpace = \raw -> raw |> skip (chompWhile \b -> b == " ")
# seed = \raw -> raw |> skipWhiteSpace
# parseSeeds = const (\n -> n)|> skip (string "seeds: ") |> sepBy (codeunit ' ') |> apply digits
parseSeeds =
    const (\n -> n)
    |> skip (string "seeds: ")
    |> keep (digits |> sepBy (codeunit ' '))
    |> skip (many (codeunit '\n'))

expect
    result = parseStr parseSeeds "seeds: 12 14 2"
    result == Ok [12, 14, 2]

skipLine = \raw -> skip raw (chompUntil '\n' |> skip (codeunit '\n'))
skipWhitespace = \raw -> skip raw (many (codeunit ' '))

parseTriple =
    const (\destination -> \source -> \size -> { destination, source, size })
    |> keep digits
    |> skipWhitespace
    |> keep digits
    |> skipWhitespace
    |> keep digits

expect
    result = parseStr parseTriple "50 98 1"
    result == Ok { destination: 50, source: 98, size: 1 }

parseMappingSection =
    const (\n -> n)
    |> skipLine
    |> keep (parseTriple |> sepBy (codeunit '\n'))
    |> skip (many (codeunit '\n'))

expect
    test =
        """
        seed-to-soil map:
        10 4 10
        11 2 5
        """
    result = parseStr parseMappingSection test
    result == Ok [{ destination: 10, source: 4, size: 10 }, { destination: 11, source: 2, size: 5 }]

parseBoard =
    const
        (\seeds -> \mappings -> {
                seeds,
                mappings,
            })
    |> keep parseSeeds
    |> keep (oneOrMore parseMappingSection)

expect
    result = parseStr
        parseBoard
        (
            example
            |> Str.split "\n"
            |> List.takeFirst 10
            |> Str.joinWith "\n"
        )
    result
    == Ok {
        seeds: [79, 14, 55, 13],
        mappings: [
            [
                { destination: 50, source: 98, size: 2 },
                { destination: 52, source: 50, size: 48 },
            ],
            [
                { destination: 0, source: 15, size: 37 },
                { destination: 37, source: 52, size: 2 },
                { destination: 39, source: 0, size: 15 },
            ],
        ],
    }

calculateLocation = \seed, mappings ->
    List.walk mappings seed \location, innerMappings ->
        List.findFirst innerMappings  \{ source, size } ->
            if location >= source then
                if location - source <= size then
                    Bool.true
                else
                    Bool.false
            else
                Bool.false
        |> Result.map \{ destination, source } -> location - source + destination
        |> Result.withDefault location

part1 = \in ->
    parseStr parseBoard in
    |> Result.try \{ seeds, mappings } ->
        seeds
        |> List.map \seed -> calculateLocation seed mappings
        |> List.min
    |> Result.withDefault 0

expect
    result = part1 example
    result == 35

part2 = \in ->
    parseStr parseBoard in
    |> Result.try \{ seeds, mappings } ->
        seeds
        |> List.chunksOf 2
        |> List.map \pair ->
            when pair is
                [start, length] -> List.range { start: At start, end: At (start + length) }
                _ -> 
                    dbg pair
                    crash "Invalid pair"
        |> List.join
        |> List.map \seed -> calculateLocation seed mappings
        |> List.min
    |> Result.withDefault 0

expect
    result = part2 example
    result == 46

example =
    """
    seeds: 79 14 55 13

    seed-to-soil map:
    50 98 2
    52 50 48

    soil-to-fertilizer map:
    0 15 37
    37 52 2
    39 0 15

    fertilizer-to-water map:
    49 53 8
    0 11 42
    42 0 7
    57 7 4

    water-to-light map:
    88 18 7
    18 25 70

    light-to-temperature map:
    45 77 23
    81 45 19
    68 64 13

    temperature-to-humidity map:
    0 69 1
    1 0 69

    humidity-to-location map:
    60 56 37
    56 93 4
    """

