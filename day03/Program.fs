open System.IO

let input = File.ReadAllLines "input.txt"

let allItems = ['a'..'z'] @ ['A'..'Z']

let findPriorityForItem item =
  Seq.findIndex ((=) item) allItems + 1

// Part 1
let findPriorityForRutsack =
  Seq.splitInto 2
  >> Seq.map Set
  >> Set.intersectMany
  >> Seq.sumBy findPriorityForItem // should only ever be 1 item in Set

input
|> Seq.sumBy findPriorityForRutsack
|> printfn "Part 1: %i"

// Part 2
input
|> Seq.chunkBySize 3
|> Seq.collect (Seq.map Set >> Set.intersectMany)
|> Seq.sumBy findPriorityForItem
|> printfn "Part 2: %i"