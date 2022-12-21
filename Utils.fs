module Utils

open System

let splitOnWithoutEmpties (delimeter: char) (str: string) =
  str.Split (delimeter, StringSplitOptions.RemoveEmptyEntries)

let splitOnWithoutEmptiesStr (delimeter: string) (str: string) =
  str.Split (delimeter, StringSplitOptions.RemoveEmptyEntries)

let tryParse parser (str: string) =
  match parser str with
  | (true, v) -> Some v
  | _ -> None

let tryParseInt = tryParse Int32.TryParse
let tryParseDateTime = tryParse DateTime.TryParse

let updateMapWith
  (fn: 'value -> 'value)
  (newValue: 'value) 
  (key: 'key) 
  (map: Map<'key, 'value>) =
    match Map.tryFind key map with
    | Some oldValue -> Map.add key (fn oldValue) map
    | None -> Map.add key newValue map

let mapCountWhere (predicate: 'key -> 'value -> bool) =
  Map.count << Map.filter predicate

module Grid =
  type Coordinate = (int * int)

  type 'a Grid = Map<Coordinate, 'a>

  let toGrid (xs: 'a seq seq) : 'a Grid =
    seq {
      for y, row in Seq.indexed xs do
      for x, v in Seq.indexed row do
        yield ((x, y), v)
    }
    |> Map.ofSeq

  let getAdjacentPoints ((x, y): Coordinate) (grid: 'a Grid) = 
    List.choose (fun coord -> Map.tryFind coord grid)
      [ (x, y-1); (x-1, y); (x+1, y); (x, y+1) ]

  let getAdjacentCoordinates ((x, y): Coordinate) (grid: 'a Grid) =
    List.filter (fun coord -> Map.containsKey coord grid)
      [ (x, y-1); (x-1, y); (x+1, y); (x, y+1) ]

  let getAdjacentPointsWithDiagonals ((x, y): Coordinate) (grid: 'a Grid) =
    List.choose (fun coord -> Map.tryFind coord grid)
      [ (x-1, y-1); (x, y-1); (x+1, y-1);
        (x-1, y);             (x+1, y);
        (x-1, y+1); (x, y+1); (x+1, y+1)
      ]

  let getAdjacentCoordinatesWithDiagonals ((x, y): Coordinate) (grid: 'a Grid) =
    List.filter (fun coord -> Map.containsKey coord grid)
      [ (x-1, y-1); (x, y-1); (x+1, y-1);
        (x-1, y);             (x+1, y);
        (x-1, y+1); (x, y+1); (x+1, y+1)
      ]

module Result =
  let failIfError =
    function
    | Ok v -> v
    | Error err -> failwith err

module List =
  let rec takeUntil endCondition =
    function
    | [] -> []
    | x::rest ->
        if endCondition x then
          [x]
        else
          x :: takeUntil endCondition rest
