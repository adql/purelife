module Gol.Canvas.Internal
       where

import Prelude

import Data.Int (floor)
import Data.Maybe (Maybe, fromJust)
import Gol.Canvas.Types (WorldGrid)
import Partial.Unsafe (unsafePartial)

unsafePointToCell :: Maybe WorldGrid -> Number -> Number -> { row::Int, col::Int }
unsafePointToCell worldGrid y x =
  let grid = (unsafePartial fromJust) worldGrid
      { cellSize } = grid
      row = floor $ y / cellSize
      col = floor $ x / cellSize
  in { row, col }
