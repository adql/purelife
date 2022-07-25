module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (error)
import Gol (play)
import Graphics.Canvas (getCanvasElementById)

main :: Effect Unit
main = do
  canvas' <- getCanvasElementById "gol"
  case canvas' of
    Nothing -> error "No canvas found"
    Just canvas -> play canvas
