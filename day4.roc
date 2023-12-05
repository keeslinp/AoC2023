app "AoC"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
    }
    imports [pf.Stdout, pf.Task.{ Task }, "day-4-input.txt" as input : Str]
    provides [main] to pf

main : Task {} *
main =
    _ <- input |> part1 |> Num.toStr |> Stdout.line |> Task.await
    input |> part2 |> Num.toStr |> Stdout.line

exampleInput =
    """
    Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
    Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
    Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
    Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
    Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
    Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
    """

parseSection = \section ->
    Str.split section " "
    |> List.keepIf \chunk -> !(Str.isEmpty chunk)
    |> List.map \n -> n |> Str.toU32 |> Result.withDefault 0

parseCard = \line ->
    line
    |> Str.split ":"
    |> List.dropFirst 1
    |> List.first
    |> Result.withDefault ""
    |> Str.trim
    |> \row ->
        when Str.split row " | " is
            [winning, actual] ->
                {
                    winning: parseSection winning,
                    actual: parseSection actual,
                }

            _ ->
                dbg
                    row

                crash "Invalid row"

expect
    card = exampleInput |> Str.split "\n" |> List.first |> Result.withDefault "" |> parseCard
    card
    == {
        winning: [41, 48, 83, 86, 17],
        actual: [83, 86, 6, 31, 17, 9, 48, 53],
    }

countMatches = \card ->
    Set.intersection (Set.fromList card.winning) (Set.fromList card.actual) |> Set.len

scoreCard = \card ->
    matchCount = countMatches card
    if matchCount > 0 then
        Num.powInt 2 (matchCount - 1)
    else
        0

expect
    score = exampleInput |> Str.split "\n" |> List.first |> Result.withDefault "" |> parseCard |> scoreCard
    score == 8

part1 = \in ->
    in
    |> Str.split "\n"
    |> List.keepIf \v -> !(Str.isEmpty v)
    |> List.map parseCard
    |> List.map scoreCard
    |> List.sum

expect
    result = part1 exampleInput
    result == 13

part2 = \in ->
    scores =
        in
        |> Str.split "\n"
        |> List.keepIf \v -> !(Str.isEmpty v)
        |> List.map parseCard
        |> List.map countMatches

    scores
    |> List.walk (0, List.repeat 0 (List.len scores)) \(total, stacks), cardMatchCount ->
        multiplier = (List.first stacks |> Result.withDefault 0) + 1
        (
            total + multiplier,
            stacks |> List.dropFirst 1 |> List.mapWithIndex \mult, index -> if index < cardMatchCount then mult + multiplier else mult,
        )
    |> .0

expect
    result = part2 exampleInput
    result == 30
