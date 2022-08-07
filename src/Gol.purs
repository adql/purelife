module Gol where

import Prelude

import Data.Int (floor, fromString, toNumber)
import Data.Maybe (Maybe(..), fromJust)
import Data.Nullable (null)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Timer (clearInterval, setInterval)
import Gol.Canvas (renderWorld)
import Gol.Logic (World, emptyWorld, tick, toggleCell, worldDimensions)
import Partial.Unsafe (unsafePartial)
import React.Basic.DOM as D
import React.Basic.DOM.Events (capture, capture_, nativeEvent, target, targetValue)
import React.Basic.Events (merge)
import React.Basic.Hooks (Component, component, readRefMaybe, useRef, useState)
import React.Basic.Hooks as React
import Utils (nodeToCanvasElement, toInterval)
import Web.CSSOM.MouseEvent (offsetX, offsetY)
import Web.DOM.Element (clientHeight, clientWidth, fromEventTarget)
import Web.UIEvent.MouseEvent (fromEvent)

type CanvasSize = { width::String, height::String }

mkGol :: Component { world::World, size::CanvasSize }
mkGol = do
  ui <- mkUI
  component "Gol" \props -> React.do
    world /\ setWorld <- useState props.world
    running /\ setRunning <- useState true
    fr /\ setFr <- useState 30
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
  
    let handleCanvasClick { target, nativeEvent } =
          let elem = unsafePartial $ fromJust $ fromEventTarget target
              mouseEvent = unsafePartial $ fromJust $ fromEvent nativeEvent
              clickY = toNumber $ offsetY mouseEvent
              clickX = toNumber $ offsetX mouseEvent
              { rows, cols } = worldDimensions world
          in do
            cvsHeight <- clientHeight elem
            cvsWidth <- clientWidth elem
            let cellHeight = cvsHeight / toNumber rows
                cellWidth = cvsWidth / toNumber cols
                row = floor $ clickY / cellHeight
                col = floor $ clickX / cellWidth
            case toggleCell world { r:row, c:col } of
              Nothing -> pure unit
              Just world' -> setWorld \_ -> world'
          
    pure $
      D.div { id:"container"
            , children:
              [ D.canvas { ref:canvas
                         , id:"gol"
                         , width:props.size.width
                         , height:props.size.height
                         , onMouseDown: capture (merge { target, nativeEvent } ) handleCanvasClick
                         }
              , ui { world, setWorld, running, setRunning, fr, setFr }
              ]
            }

mkUI :: Component
        { world :: World
        , setWorld :: (World -> World) -> Effect Unit
        , running :: Boolean
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
                      }
            , D.button { onClick: capture_ $ props.setWorld \_ ->
                          emptyWorld $ worldDimensions props.world
                       , children: [ D.text "Clear"]
                       }
            ]}

    
