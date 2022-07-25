module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (error)
import Graphics.Canvas (getCanvasElementById)
import UI (drawGrid, mkWorldGrid)

main :: Effect Unit
main = do
  canvas' <- getCanvasElementById "gol"
  case canvas' of
    Nothing -> error "No canvas found"
    Just canvas -> mkWorldGrid canvas 50 >>= drawGrid
