module Gol.Logic.Types where

import Data.Array2D (Array2D)
import Data.Eq (class Eq)

type World = Array2D Cell

type WorldDimensions = { rows::Int, cols::Int }

data Cell = Alive | Dead

derive instance eqCell :: Eq Cell
