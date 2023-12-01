app "AoC"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [pf.Stdout, pf.Task.{Task}, "day-1-input.txt" as input : Str]
    provides [main] to pf

main : Task {} *
main =
    _ <- input |> part1 |> Num.toStr |> Stdout.line |> Task.await
    input |> part2 |> Num.toStr |> Stdout.line

combine = \first, second ->
    when (first, second) is
        (Some f, Some s) -> f * 10 + s
        (Some f, None) -> f * 10 + f
        _ -> 0

walk = \(sum, first, second), scalar ->
    when (scalar, first, second) is
        # newline
        (10, f, s) -> (sum + combine f s, None, None)
        # digit
        (n, Some f, _) if (n >= 48) && (n <= 57) -> (sum, Some f, Some (n - 48)) 
        (n, None, _) if (n >= 48) && (n <= 57) -> (sum, Some (n - 48), None) 
        # else
        _ -> (sum, first, second)
        

part1 : Str -> U32
part1 = \in ->
    in
    |> Str.walkScalars (0, None, None) walk |> \(sum, first, second) -> sum + combine first second

part1Example =
    """
    1abc2
    pqr3stu8vwx
    a1b2c3d4e5f
    treb7uchet
    """

expect
    part1Example |> part1 == 142

replaceStringNums = \str -> 
    str
    |> Str.replaceEach "one" "one1one"
    |> Str.replaceEach "two" "two2two"
    |> Str.replaceEach "three" "three3three"
    |> Str.replaceEach "four" "four4four"
    |> Str.replaceEach "five" "five5five"
    |> Str.replaceEach "six" "six6six"
    |> Str.replaceEach "seven" "seven7seven"
    |> Str.replaceEach "eight" "eight8eight"
    |> Str.replaceEach "nine" "nine9nine"

part2Example = 
    """
    two1nine
    eightwothree
    abcone2threexyz
    xtwone3four
    4nineeightseven2
    zoneight234
    7pqrstsixteen
    """

part2 = \in ->
    in |> replaceStringNums |> part1

expect part2 part2Example == 281
