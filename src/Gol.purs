module Gol where

import Prelude

import Data.Array2D (replicate2D)
import Data.Maybe (Maybe(..))
import Data.Nullable (null)
import Data.Traversable (sequence)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Random (random)
import Effect.Timer (clearInterval, setInterval)
import Gol.Logic (World, tick)
import Gol.Render (mkWorldGrid, renderWorld)
import React.Basic.DOM as D
import React.Basic.Hooks (Component, component, readRefMaybe, useRef, useState)
import React.Basic.Hooks as React
import Utils (nodeToCanvasElement)

type CanvasSize = { width::String, height::String }

emptyWorld :: Int -> Int -> World
emptyWorld r c = replicate2D r c false

randomWorld :: Int -> Int -> Number -> Effect World
randomWorld r c p = sequence $ replicate2D r c (map (_ < p) random)

mkGol :: World -> CanvasSize -> Component Unit
mkGol world0 size = do
  component "Gol" \_ -> React.do
    world /\ setWorld <- useState world0
    canvas <- useRef null
  
    React.useEffect world do
      current <- readRefMaybe canvas
      case current of
        Nothing -> pure mempty
        Just node -> do
          grid <- mkWorldGrid (nodeToCanvasElement node) 50
          renderWorld grid world
          pure mempty

    React.useEffect unit do
      intervalId <- setInterval 50 $ setWorld (\w -> tick w)
      pure $ clearInterval intervalId

    pure $ D.canvas { ref:canvas
                    , id:"gol"
                    , width:size.width
                    , height:size.height
                    }
