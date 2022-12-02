open System.IO

let input = File.ReadAllLines "input.txt"

let findOurTotalScore roundFn = 
  input |> Seq.sumBy roundFn

// A, X => Rock (1) 
// B, Y => Paper (2)
// C, Z => Scissors (3)

// Part 1
let playRoundPart1 = 
  function
  // Draws 
  | "A X" -> 4
  | "B Y" -> 5
  | "C Z" -> 6

  // Opponent wins
  | "A Z" -> 3
  | "B X" -> 1
  | "C Y" -> 2

  // Us win
  | "C X" -> 7
  | "A Y" -> 8
  | "B Z" -> 9

  | other -> failwith $"Unexpected round %s{other}"

findOurTotalScore playRoundPart1 
|> printfn "Part 1: %i"

// Part 2

// X => lose
// Y => draw
// Z => win

let playRoundPart2 =
  function
  // Rocks
  | "A X" -> 3
  | "A Y" -> 4
  | "A Z" -> 8

  // Papers
  | "B X" -> 1
  | "B Y" -> 5
  | "B Z" -> 9

  // Scissors
  | "C X" -> 2
  | "C Y" -> 6
  | "C Z" -> 7

  | other -> failwith $"Unexpected round %s{other}"
  
findOurTotalScore playRoundPart2
|> printfn "Part 2: %i"