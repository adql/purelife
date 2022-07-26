module Gol where

import Prelude

import Data.Array2D (replicate2D)
import Data.Maybe (Maybe(..))
import Data.Nullable (null)
import Data.Traversable (sequence)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Random (random)
import Gol.Logic (World)
import Gol.Render (mkWorldGrid, renderWorld)
import Graphics.Canvas (CanvasElement)
import React.Basic.DOM as D
import React.Basic.Hooks (Component, component, readRefMaybe, useRef, useState)
import React.Basic.Hooks as React
import Utils (nodeToCanvasElement)

emptyWorld :: Int -> Int -> World
emptyWorld r c = replicate2D r c false

randomWorld :: Int -> Int -> Number -> Effect World
randomWorld r c p = sequence $ replicate2D r c (map (_ < p) random)

mkGol :: World -> Component Unit
mkGol world0 = do
  component "Gol" \_ -> React.do
    world /\ setWorld <- useState world0
    canvas <- useRef null
  
    React.useEffect unit do
      current <- readRefMaybe canvas
      case current of
        Nothing -> pure mempty
        Just node -> do
          grid <- mkWorldGrid (nodeToCanvasElement node) 50
          renderWorld grid world
          pure mempty

    pure $ D.canvas { ref:canvas
                    , width:"600"
                    , height:"600"
                    }

play :: CanvasElement -> Effect Unit
play canvas = do
  world <- randomWorld 50 50 0.4
  grid <- mkWorldGrid canvas 50
  renderWorld grid world
