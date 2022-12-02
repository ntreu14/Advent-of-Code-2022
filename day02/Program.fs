open System.IO

let input = File.ReadAllLines "input.txt"

let findOurTotalScore roundFn = 
  input |> Array.fold roundFn 0

// A, X => Rock (1) 
// B, Y => Paper (2)
// C, Z => Scissors (3)

// Part 1
let playRoundPart1 runningScore = 
  function
  // Draws 
  | "A X" -> runningScore + 1 + 3
  | "B Y" -> runningScore + 2 + 3
  | "C Z" -> runningScore + 3 + 3

  // Opponent wins
  | "A Z" -> runningScore + 3
  | "B X" -> runningScore + 1
  | "C Y" -> runningScore + 2

  // Us win
  | "C X" -> runningScore + 1 + 6
  | "A Y" -> runningScore + 2 + 6
  | "B Z" -> runningScore + 3 + 6

  | other -> failwith $"Unexpected round %A{other}"

findOurTotalScore playRoundPart1 
|> printfn "Part 1: %i"

// Part 2

// X => lose
// Y => draw
// Z => win

let playRoundPart2 runningScore =
  function
  // Rocks
  | "A X" -> runningScore + 3
  | "A Y" ->  runningScore + 1 + 3
  | "A Z" -> runningScore + 2 + 6

  // Papers
  | "B X" -> runningScore + 1
  | "B Y" -> runningScore + 2 + 3
  | "B Z" -> runningScore + 3 + 6

  // Scissors
  | "C X" -> runningScore + 2
  | "C Y" -> runningScore + 3 + 3
  | "C Z" -> runningScore + 1 + 6

  | other -> failwith $"Unexpected round %A{other}"
  
findOurTotalScore playRoundPart2
|> printfn "Part 2: %i"