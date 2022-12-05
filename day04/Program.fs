open System.IO
open Utils

let input = File.ReadAllLines "input.txt"

let isSectionOverlap sectionsPredicate (line: string) =
  match line.Replace('-', ',') |> splitOnWithoutEmpties ',' with
  | [| lower1; upper1; lower2; upper2 |] -> 
      let sections1 = Set.ofList [int lower1 .. int upper1]
      let sections2 = Set.ofList [int lower2 .. int upper2]

      sectionsPredicate sections1 sections2

  | other -> failwith $"Cannot parse: %A{other}"

let solve sectionsPredicate =
  input
  |> Seq.filter (isSectionOverlap sectionsPredicate)
  |> Seq.length

// Part 1
let isFullOverlap sections1 sections2 =
  Set.isSubset sections1 sections2 || Set.isSubset sections2 sections1  
  
solve isFullOverlap 
|> printfn "Part 1: %i"

// Part 2
let isAnyOverLap sections1 sections2 = 
  Set.intersect sections1 sections2
  |> Set.isEmpty
  |> not

solve isAnyOverLap
|> printfn "Part 2: %i"