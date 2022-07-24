module Game where

import Prelude

import Data.Array2D (Array2D, Index2D, index2D, mapWithIndex2D, replicate2D)
import Data.Foldable (sum)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Random (random)

type World = Array2D Boolean

pronounce :: Boolean -> Int -> Boolean
pronounce alive 2 = alive
pronounce _     3 = true
pronounce _     _ = false

countNeighbors :: World -> Index2D -> Int
countNeighbors w {r,c} = sum $ map knock neighbors
  where
    knock :: Index2D -> Int
    knock i = case index2D w i of
      Nothing    -> 0
      Just false -> 0
      Just true  -> 1
    neighbors :: Array Index2D
    -- probably more efficient than removing an element from an array
    neighbors = [ { r:(r-1), c:(c-1) } , { r:(r-1), c:c } , { r:(r-1), c:(c+1) }
                , { r: r   , c:(c-1) }                    , { r: r   , c:(c+1) }
                , { r:(r+1), c:(c-1) } , { r:(r+1), c:c } , { r:(r+1), c:(c+1) }
                ]

tick :: World -> World
tick w = mapWithIndex2D (\i x -> pronounce x $ countNeighbors w i) w

emptyWorld :: Int -> Int -> World
emptyWorld r c = replicate2D r c false

randomWorld :: Int -> Int -> Number -> Effect World
randomWorld r c p = sequence $ replicate2D r c (map (_ < p) random)
