module Utils

open System

type Coordinate = (int * int)

let toCoordinateMap (xs: 'a seq seq) : Map<Coordinate, 'a> =
  seq {
    for y, row in Seq.indexed xs do
    for x, v in Seq.indexed row do
      yield ((x, y), v)
  }
  |> Map.ofSeq

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

let getAdjacentPoints ((x, y): Coordinate) (grid: Map<Coordinate, 'a>) = 
  List.choose (fun coord -> Map.tryFind coord grid)
    [ (x, y-1); (x-1, y); (x+1, y); (x, y+1) ]

let getAdjacentCoordinates ((x, y): Coordinate) (grid: Map<Coordinate, 'a>) =
  List.filter (fun coord -> Map.containsKey coord grid)
    [ (x, y-1); (x-1, y); (x+1, y); (x, y+1) ]

let getAdjacentPointsWithDiagonals ((x, y): Coordinate) (grid: Map<Coordinate, 'a>) =
  List.choose (fun coord -> Map.tryFind coord grid)
    [ (x-1, y-1); (x, y-1); (x+1, y-1);
      (x-1, y);             (x+1, y);
      (x-1, y+1); (x, y+1); (x+1, y+1)
    ]

let getAdjacentCoordinatesWithDiagonals ((x, y): Coordinate) (grid: Map<Coordinate, 'a>) =
  List.filter (fun coord -> Map.containsKey coord grid)
    [ (x-1, y-1); (x, y-1); (x+1, y-1);
      (x-1, y);             (x+1, y);
      (x-1, y+1); (x, y+1); (x+1, y+1)
    ]

let splitOnWithoutEmpties (str: string) (delimeter: char) =
  str.Split (delimeter, StringSplitOptions.RemoveEmptyEntries)

let splitOnWithoutEmptiesStr (str: string) (delimeter: string) =
  str.Split (delimeter, StringSplitOptions.RemoveEmptyEntries)

let tryParse parser (str: string) =
  match parser str with
  | (true, v) -> Some v
  | _ -> None

let tryParseInt = tryParse Int32.TryParse