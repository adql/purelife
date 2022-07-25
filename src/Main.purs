module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (error)
import Graphics.Canvas (getCanvasElementById)
import UI (drawCell, drawGrid, mkWorldGrid)

main :: Effect Unit
main = do
  canvas' <- getCanvasElementById "gol"
  case canvas' of
    Nothing -> error "No canvas found"
    Just canvas -> do
      worldGrid <- mkWorldGrid canvas 50
      drawGrid worldGrid
      drawCell worldGrid 5 10 true
      drawCell worldGrid 0 0 true
      drawCell worldGrid 0 0 false -- erasing?
