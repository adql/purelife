module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (error, log)
import Graphics.Canvas (getCanvasElementById)
import UI (drawGrid)
import Web.HTML (window)
import Web.HTML.Window (document)

main :: Effect Unit
main = do
  doc <- document =<< window
  canvas' <- getCanvasElementById "gol"
  case canvas' of
    Nothing -> error "No canvas found"
    Just canvas -> drawGrid canvas 50
