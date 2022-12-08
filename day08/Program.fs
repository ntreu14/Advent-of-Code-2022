open System.IO
open Utils

let trees = 
  File.ReadAllLines "input.txt"
  |> Seq.map (Seq.map (string >> int))
  |> Grid.toGrid

let maxX, maxY = Map.maxKeyValue trees |> fst

let findAllTreesToCheck (x, y) = 
  seq {
    yield seq { for x' in [x .. -1 .. 0] do if (x', y) <> (x, y) then yield (x', y) } // lefts
    yield seq { for x' in [x..maxY] do if (x', y) <> (x, y) then yield (x', y) }      // rights
    yield seq { for y' in [y .. -1 .. 0] do if (x, y') <> (x, y) then yield (x, y') } // top
    yield seq { for y' in [y..maxY] do if (x, y') <> (x, y) then yield (x, y') }      // bottom
  }

// Part 1
let isTallestTree (x, y) height = 
  if x = 0 || x = maxX || y = 0 || y = maxY then
    true
  else
    findAllTreesToCheck (x, y)
    |> Seq.map (fun direction ->
        direction
        |> Seq.filter (fun c -> 
          Map.tryFind c trees
          |> Option.filter (fun t -> t >= height)
          |> Option.isSome)
    )
    |> Seq.exists Seq.isEmpty
      
trees
|> Map.filter isTallestTree
|> Map.count
|> printfn "Part 1: %i"

// Part 2
let findScenicScore ((x, y), height) =
  let findCountOfTreesInDirection product direction = 
    direction
    |> List.ofSeq
    |> List.takeUntil (fun c -> 
      Map.tryFind c trees
      |> Option.filter (fun t -> height <= t)
      |> Option.isSome
    )
    |> Seq.length
    |> (*) product

  findAllTreesToCheck (x, y)
  |> Seq.fold findCountOfTreesInDirection 1

trees
|> Map.toSeq
|> Seq.map findScenicScore
|> Seq.max
|> printfn "Part 2: %i"