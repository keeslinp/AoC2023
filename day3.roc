app "AoC"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.2.0/dJQSsSmorujhiPNIvJKlQoI92RFIG_JQwUfIxZsCSwE.tar.br",
    }
    imports [pf.Stdout, pf.Task.{Task}, "day-3-input.txt" as input : Str]
    provides [main] to pf

main : Task {} *
main =
    _ <- input |> part1 |> Num.toStr |> Stdout.line |> Task.await
    input |> part2 |> Num.toStr |> Stdout.line

part1Example =
  """
  467..114..
  ...*......
  ..35..633.
  ......#...
  617*......
  .....+.58.
  ..592.....
  ......755.
  ...$.*....
  .664.598..
  """
expect
  result = part1 part1Example
  result == 4361

buildGrid = \in ->
  in
  |> Str.graphemes
  |> List.map \n ->
    when n is
      "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> Num (Str.toNat n |> Result.withDefault 0)
      "\n" -> NewLine
      "." -> Empty
      c -> Symbol c

expect
  grid = "467..114..\n...*......." |> buildGrid
  grid
    ==
    [
      Num 4, Num 6, Num 7, Empty, Empty, Num 1, Num 1, Num 4, Empty, Empty, NewLine,
      Empty, Empty, Empty, Symbol "*", Empty, Empty, Empty,Empty,Empty,Empty,Empty
    ]

calcWidth = \grid ->
  List.walkUntil grid 0 \count, cell ->
    if cell == NewLine then
      Break (count + 1) # We want to include the newline
    else
      Continue (count + 1)

expect
  width = part1Example |> buildGrid |> calcWidth
  width == 11

getNeighborIndices = \n, width ->
  [
    if n > width + 1 then n - width - 1 else 0,
    if n > width then n - width else 0,
    if n > width - 1 then n - width + 1 else 0,
    if n > 1 then n - 1 else 0,
    n, n + 1, n + width - 1, n + width, n + width + 1
  ]


getNeighbors = \n, width, grid ->
  getNeighborIndices n width
  |> List.map \index -> index |> Num.max 0
  |> List.map \index -> List.get grid index |> Result.withDefault Empty


getNumberIndices = \grid ->
  grid
  |> List.walkWithIndex ([], None)
    \(numbers, current), cell, index ->
      when (cell, current) is
        (Num _, Some list) -> (numbers, Some (List.append list index))
        (Num _, None) -> (numbers, Some [index])
        (_, Some list) -> (List.append numbers list, None)
        (_, _) -> (numbers, None)
  |> \(numbers, current) ->
    when current is
      Some list -> List.append numbers list
      None -> numbers

expect
  numbers = part1Example |> Str.split "\n" |> List.takeFirst 2 |> Str.joinWith "\n" |> buildGrid |> getNumberIndices
  numbers == [
    [0, 1, 2],
    [5, 6, 7]
  ]

hasSymbolNeighbor = \index, width, grid ->
  getNeighbors index width grid
  |> List.any \c ->
    when c is
      Symbol _ -> Bool.true
      _ -> Bool.false

filterAdjacent = \numbers, grid ->
  width = calcWidth grid
  numbers
  |> List.keepIf \number -> List.any number \index -> hasSymbolNeighbor index width grid

expect
  grid = part1Example |> Str.split "\n" |> List.takeFirst 3 |> Str.joinWith "\n" |> buildGrid 
  numbers = grid |> getNumberIndices |> filterAdjacent grid
  numbers
    ==
    [[0, 1, 2], [24, 25]]

calcPartNumber = \numberIndices, grid ->
  List.walk numberIndices 0 \result, index ->
    result * 10 +
    (List.get grid index
      |> Result.map \cell -> 
        when cell is
          Num n -> n
          _ -> crash "Bad cell in num"
      |> Result.withDefault 0
    )
    
expect
  grid = part1Example |> Str.split "\n" |> List.takeFirst 3 |> Str.joinWith "\n" |> buildGrid 
  num = calcPartNumber [0, 1, 2] grid
  num == 467

part1 = \in ->
  grid = in |> buildGrid
  grid
  |> getNumberIndices
  |> filterAdjacent grid
  |> List.map \n -> calcPartNumber n grid
  |> List.sum

gearIndices = \grid ->
  List.walkWithIndex grid [] \list, cell, index ->
    when cell is
      Symbol "*" -> List.append list index
      _ -> list

expect
  indices = part1Example |> buildGrid |> gearIndices
  List.len indices == 3

partsNextToGear = \numbers, grid ->
  width = calcWidth grid
  gearNeighbors = gearIndices grid
    |> List.map
      \gearIndex -> getNeighborIndices gearIndex width
  gearNeighbors
  |> List.map \validIndices -> 
    numbers
    |> List.keepIf
      \number -> List.any number
        \index -> List.contains validIndices index

expect
  grid = part1Example |> Str.split "\n" |> List.takeFirst 3 |> Str.joinWith "\n" |> buildGrid 
  num = calcPartNumber [0, 1, 2] grid
  num == 467

part2 = \in ->
  grid = in |> buildGrid
  grid
  |> getNumberIndices
  |> partsNextToGear grid
  |> List.keepIf \neighbors -> List.len neighbors == 2
  |> List.map
    \parts -> List.walk parts 1
      \product, num -> product * (calcPartNumber num grid)
  |> List.sum

expect
  result = part2 part1Example
  result == 467835
