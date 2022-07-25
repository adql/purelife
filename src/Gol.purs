module Gol where

import Prelude

import Data.Array2D (replicate2D)
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Random (random)
import Gol.Logic (World)

emptyWorld :: Int -> Int -> World
emptyWorld r c = replicate2D r c false

randomWorld :: Int -> Int -> Number -> Effect World
randomWorld r c p = sequence $ replicate2D r c (map (_ < p) random)
