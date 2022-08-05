module Gol where

import Prelude

import Data.Array2D (replicate2D)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Nullable (null)
import Data.Traversable (sequence)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Random (random)
import Effect.Timer (clearInterval, setInterval)
import Gol.Logic (World, tick)
import Gol.Render (renderWorld)
import React.Basic.DOM as D
import React.Basic.DOM.Events (capture, capture_, targetValue)
import React.Basic.Hooks (Component, component, readRefMaybe, useRef, useState)
import React.Basic.Hooks as React
import Utils (nodeToCanvasElement, toInterval)

type CanvasSize = { width::String, height::String }

emptyWorld :: Int -> Int -> World
emptyWorld r c = replicate2D r c false

randomWorld :: Int -> Int -> Number -> Effect World
randomWorld r c p = sequence $ replicate2D r c (map (_ < p) random)

mkGol :: Component { world::World, size::CanvasSize }
mkGol = do
  ui <- mkUI
  component "Gol" \props -> React.do
    world /\ setWorld <- useState props.world
    running /\ setRunning <- useState true
    fr /\ setFr <- useState 5
    canvas <- useRef null
  
    React.useEffect world do
      current <- readRefMaybe canvas
      case current of
        Nothing -> pure mempty
        Just node -> do
          renderWorld (nodeToCanvasElement node) world
          pure mempty

    React.useEffect { running, fr } $
      if running then do
        intervalId <- setInterval (toInterval fr) $ setWorld (\w -> tick w)
        pure $ clearInterval intervalId
      else pure mempty
  
    pure $
      D.div { id:"container"
            , children:
              [ D.canvas { ref:canvas
                         , id:"gol"
                         , width:props.size.width
                         , height:props.size.height
                         }
              , ui { running, setRunning, fr, setFr }
              ]
            }

mkUI :: Component
        { running :: Boolean
        , setRunning :: (Boolean -> Boolean) -> Effect Unit
        , fr :: Int
        , setFr :: (Int -> Int) -> Effect Unit
        }
mkUI = do
  component "UI" \props -> pure $
    D.div { id:"ui"
          , children:
            [ D.button { onClick: capture_ $ props.setRunning $ \r -> not r
                        , children: [ D.text $ if props.running then "Stop" else "Start" ] }
            , D.input { type:"range"
                      , min:"1"
                      , max:"100"
                      , defaultValue:show props.fr
                      , onChange: capture targetValue $ \v -> case map fromString v of
                          Nothing -> pure unit
                          Just Nothing -> pure unit
                          Just (Just rate) -> props.setFr ( \_ -> rate ) *>
                                              props.setRunning \r -> not $ not r
                        }]}

    
