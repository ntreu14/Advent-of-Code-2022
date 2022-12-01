open System.IO
open Utils

let parseElfCalories (elfTotal, elvesTotals) =
  function
  | "" -> (0, elfTotal :: elvesTotals)
  | line ->
    let calories = tryParseInt line |> Option.defaultValue 0
    (calories + elfTotal, elvesTotals)

let elvesCalories = 
  File.ReadAllLines "input.txt"
  |> Array.fold parseElfCalories (0, [])
  |> snd

// Part 1
List.max elvesCalories
|> printfn "Part 1: %i"

// Part 2
List.sortDescending elvesCalories
|> List.take 3
|> List.sum
|> printfn "Part 2: %i"