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

let splitOnWithoutEmpties (str: string) (delimeter: char) =
  str.Split (delimeter, StringSplitOptions.RemoveEmptyEntries)

let splitOnWithoutEmptiesStr (str: string) (delimeter: string) =
  str.Split (delimeter, StringSplitOptions.RemoveEmptyEntries)

let tryParse parser (str: string) =
  match parser str with
  | (true, v) -> Some v
  | _ -> None

let tryParseInt = tryParse Int32.TryParse