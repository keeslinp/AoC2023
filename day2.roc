app "AoC"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.2.0/dJQSsSmorujhiPNIvJKlQoI92RFIG_JQwUfIxZsCSwE.tar.br",
    }
    imports [pf.Stdout, pf.Task.{ Task }, "day-2-input.txt" as input : Str]
    provides [main] to pf

main : Task {} *
main =
    _ <- input |> part1 |> Num.toStr |> Stdout.line |> Task.await
    input |> part2 |> Num.toStr |> Stdout.line

exampleInput =
    """
    Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
    Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
    Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
    Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
    Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
    """

limit = {
    r: 12,
    g: 13,
    b: 14,
}

parseRounds = \rounds ->
    rounds
    |> Str.split ";"
    |> List.map Str.trim
    |> List.map
        (\n -> Str.split n ", "
            |> List.walk
                { r: None, g: None, b: None }
                \acc, group ->
                    when Str.split group " " is
                        [val, "blue"] -> { acc & b: Some (Str.toU32 val |> Result.withDefault 0) }
                        [val, "green"] -> { acc & g: Some (Str.toU32 val |> Result.withDefault 0) }
                        [val, "red"] -> { acc & r: Some (Str.toU32 val |> Result.withDefault 0) }
                        _ -> crash "Bad value"
        )

expect parseRounds "3 blue, 4 red" == [{ b: Some 3, r: Some 4, g: None }]
expect parseRounds "1 blue; 2 red; 4 green" == [{ b: Some 1, r: None, g: None }, { b: None, r: Some 2, g: None }, { b: None, r: None, g: Some 4 }]

getNumOr0 = \n ->
    when n is
        Some v -> v
        None -> 0

reduceRoundsToMax = \rounds ->
    List.walk rounds { r: 0, g: 0, b: 0 } \maxes, round -> {
        r: Num.max (getNumOr0 round.r) maxes.r,
        g: Num.max (getNumOr0 round.g) maxes.g,
        b: Num.max (getNumOr0 round.b) maxes.b,
    }

expect
    reduceRoundsToMax
        [{ b: Some 1, r: Some 3, g: None }, { b: None, r: Some 2, g: None }, { b: Some 2, r: None, g: Some 4 }]
    ==
    {
        r: 3,
        g: 4,
        b: 2,
    }

debug = \v ->
    dbg
        v

    v

parseGame = \rawLine ->
    when Str.split rawLine ":" is
        [game, rounds] ->
            {
                gameIndex: Str.split game " " |> List.get 1 |> Result.try Str.toNat |> Result.withDefault 0,
                largest: rounds |> parseRounds |> reduceRoundsToMax,
            }

        _ -> crash "invalid"

isValidGame = \game -> game.largest.r <= limit.r && game.largest.g <= limit.g && game.largest.b <= limit.b

getScore = .gameIndex

part1 = \in ->
    in
    |> Str.split "\n"
    |> List.keepIf (\r -> r != "")
    |> List.map parseGame
    |> List.keepIf isValidGame
    |> List.map getScore
    |> List.sum

expect
    result = part1 exampleInput
    result == 8

calcPower = \game ->
    (Num.max game.largest.r 1)
    * (Num.max game.largest.g 1)
    * (Num.max game.largest.b 1)

part2 = \in ->
    in
    |> Str.split "\n"
    |> List.keepIf (\r -> r != "")
    |> List.map parseGame
    |> List.map calcPower
    |> List.sum
expect part2 exampleInput == 2286
