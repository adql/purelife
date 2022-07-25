module Gol where

import Prelude

import Data.Array2D (replicate2D)
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Random (random)
import Gol.Logic (World)
import Gol.Render (mkWorldGrid, renderWorld)
import Graphics.Canvas (CanvasElement)

emptyWorld :: Int -> Int -> World
emptyWorld r c = replicate2D r c false

randomWorld :: Int -> Int -> Number -> Effect World
randomWorld r c p = sequence $ replicate2D r c (map (_ < p) random)

play :: CanvasElement -> Effect Unit
play canvas = do
  world <- randomWorld 50 50 0.4
  grid <- mkWorldGrid canvas 50
  renderWorld grid world
