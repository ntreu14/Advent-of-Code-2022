open System.IO
open System.Collections.Generic
open Utils

let rawInstructions = File.ReadAllLines "instructions.txt"
let rawStacks = File.ReadAllLines "stacks.txt" |> Seq.rev // Reverse to get stack order correct

type Instruction = {
  MoveCrateCount: int
  FromStack: int
  ToStack: int
}

let parseInstrcutions (line: string) = 
  match splitOnWithoutEmptiesStr " " line with
  | [|"move"; crateCount; "from"; fromStack; "to"; toStack |] ->
    
    { MoveCrateCount=int crateCount
      FromStack=int fromStack 
      ToStack=int toStack }    
  
  | other -> failwith $"Cannot parse instruction: %A{other}"

let addCrateToStack (stacks: Map<int, string Stack>) (stack, crate) =
  updateMapWith
    (fun (s: string Stack) -> s.Push crate; s)
    (Stack [crate])
    stack
    stacks

let parseStacks accStacks (line: string) =
  let maybeParseCrate i (maybeCrate: string) =
    match maybeCrate.Replace("[", "").Replace("]", "") with
    | "-" -> None
    | crate -> Some (i + 1, crate)

  splitOnWithoutEmptiesStr " " line
  |> Seq.mapi maybeParseCrate
  |> Seq.choose id
  |> Seq.fold addCrateToStack accStacks

// For part 2 solution, uncomment the `|> Seq.rev` in the function below.
let runInstructions (stacks: Map<int, string Stack>) (instruction: Instruction) =
  seq {
      for _ = 1 to instruction.MoveCrateCount do
        yield!
          Map.tryFind instruction.FromStack stacks
          |> Option.map (fun s -> s.Pop())
          |> Option.toList
    }
    //|> Seq.rev
    |> Seq.fold (fun s crate -> addCrateToStack s (instruction.ToStack, crate)) stacks

let findTopCrates (stacks: Map<int, string Stack>) =
  seq { 
    for stack in Map.keys stacks do // we know there are 9 stacks
      yield
        Map.find stack stacks 
        |> fun s -> s.Peek()
  }
  |> String.concat ""

let instructions = Seq.map parseInstrcutions rawInstructions
let stacks = Seq.fold parseStacks Map.empty rawStacks

instructions 
|> Seq.fold runInstructions stacks
|> findTopCrates
|> printfn "%s"