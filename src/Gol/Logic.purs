module Gol.Logic
       ( emptyWorld
       , randomWorld
       , tick
       , getCell
       , setCell
       , toggle
       , worldDimensions
       , module Gol.Logic.Types
       )
       where

import Prelude

import Data.Array2D (Index2D, dimensions, index2D, mapWithIndex2D, modifyAt2D, replicate2D)
import Data.Foldable (sum)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Random (random)
import Gol.Logic.Types (Cell(..), World, WorldDimensions)

pronounce :: Cell -> Int -> Cell
pronounce alive 2 = alive
pronounce _     3 = Alive
pronounce _     _ = Dead

getCell :: World -> Index2D -> Maybe Cell
getCell = index2D

setCell :: Cell -> World -> Index2D -> Maybe World
setCell s w i = modifyAt2D i (\_ -> s) w

toggle :: Cell -> Cell
toggle Alive = Dead
toggle Dead = Alive

countNeighbors :: World -> Index2D -> Int
countNeighbors w {r,c} = sum $ map knock neighbors
  where
    neighbors :: Array Index2D
    -- probably more efficient than removing an element from an array
    neighbors = [ { r:(r-1), c:(c-1) } , { r:(r-1), c:c } , { r:(r-1), c:(c+1) }
                , { r: r   , c:(c-1) }                    , { r: r   , c:(c+1) }
                , { r:(r+1), c:(c-1) } , { r:(r+1), c:c } , { r:(r+1), c:(c+1) }
                ]

    { rows, cols } = worldDimensions w
    wrapR r0 = r0 `mod` rows
    wrapC c0 = c0 `mod` cols

    knock :: Index2D -> Int
    knock i@{r,c} = case getCell w i of
      Nothing -> knock { r: wrapR r, c: wrapC c }
      Just Dead -> 0
      Just Alive -> 1

tick :: World -> World
tick w = mapWithIndex2D (\i x -> pronounce x $ countNeighbors w i) w

emptyWorld :: WorldDimensions -> World
emptyWorld { rows, cols } = replicate2D rows cols Dead

randomWorld :: WorldDimensions -> Number -> Effect World
randomWorld { rows, cols } p = sequence $ replicate2D rows cols (map f random)
  where
    f rnd = if rnd < p then Alive else Dead

worldDimensions :: World -> WorldDimensions
worldDimensions = dimensions
