{-# LANGUAGE OverloadedStrings #-}
module Utils
  ( Coordinate
  , readFileLines
  , toCoordinateMap
  , getAdjacentPoints
  , getAdjacentCoordinates
  , getAdjacentPointsWithDiagonals
  , getAdjacentCoordinatesWithDiagonals
  , toInts
  , trd 
  , mapBoth
  , countOccurrences
  , textToInt
  ) where

import Data.Maybe (mapMaybe)
import Data.Char (digitToInt)
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Map.Strict as M
import qualified Data.Text       as T

type Coordinate = (Int, Int)

readFileLines :: FilePath -> IO [String]
readFileLines filePath = lines <$> readFile filePath

toCoordinateMap :: [[a]] -> Map Coordinate a
toCoordinateMap xs = M.fromList $ do
  (y, row) <- zip [0 ..] xs
  (x, v) <- zip [0 ..] row
  pure ((x, y), v)

getAdjacentPoints :: (Ord a1, Ord a2, Num a1, Num a2) 
                  => (a1, a2) 
                  -> Map (a1, a2) b 
                  -> [b]
getAdjacentPoints (x, y) grid =
  mapMaybe (`M.lookup` grid)
    [ (x, y-1), (x-1, y), (x+1, y), (x, y+1)
    ]

getAdjacentCoordinates :: (Ord a1, Ord a2, Num a2, Num a1) 
                        => (a1, a2) 
                        -> Map (a1, a2) a3 
                        -> [(a1, a2)]
getAdjacentCoordinates (x, y) grid =
  filter (`M.member` grid) 
    [ (x, y-1), (x-1, y), (x+1, y), (x, y+1)
    ]

getAdjacentPointsWithDiagonals :: (Ord a1, Ord a2, Num a1, Num a2) 
                               => (a1, a2) 
                               -> Map (a1, a2) b 
                               -> [b]
getAdjacentPointsWithDiagonals (x, y) grid =
  mapMaybe (`M.lookup` grid)
    [ (x-1, y-1), (x, y-1), (x+1, y-1)
    , (x-1, y),             (x+1, y) 
    , (x-1, y+1), (x, y+1), (x+1, y+1)
    ]

getAdjacentCoordinatesWithDiagonals :: (Ord a1, Ord a2, Num a1, Num a2) 
                                    => (a1, a2) 
                                    -> Map (a1, a2) a3 
                                    -> [(a1, a2)]
getAdjacentCoordinatesWithDiagonals (x, y) grid =
  filter (`M.member` grid)
    [ (x-1, y-1), (x, y-1), (x+1, y-1)
    , (x-1, y),             (x+1, y) 
    , (x-1, y+1), (x, y+1), (x+1, y+1)
    ]

toInts :: [String] -> [[Int]]
toInts = map $ map digitToInt

trd :: (a, b, c) -> c
trd (_, _, c) = c

mapBoth :: (a -> b) -> (a, a) -> (b, b)
mapBoth f (a, b) = (f a, f b)

countOccurrences :: (Ord k, Num a) => [k] -> Map k a
countOccurrences str = M.fromListWith (+) [(c, 1) | c <- str]

textToInt :: Text -> Int
textToInt = read . T.unpack