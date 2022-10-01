 module Gol where

import Prelude

import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromJust)
import Data.Nullable (null)
import Data.Tuple.Nested ((/\))
import Effect.Timer (clearInterval, setInterval)
import Gol.Canvas (CanvasSize, WorldGrid, renderWorld)
import Gol.Canvas.Internal (unsafePointToCell)
import Gol.Logic (Cell(..), World, getCell, setCell, tick, toggle)
import Partial.Unsafe (unsafePartial)
import React.Basic.DOM as D
import React.Basic.DOM.Events (capture, nativeEvent)
import React.Basic.Hooks (Component, component, readRefMaybe, useEffect, useRef, useState)
import React.Basic.Hooks as React
import UI (mkUI)
import Utils (nodeToCanvasElement, toInterval)
import Web.CSSOM.MouseEvent (offsetX, offsetY)
import Web.UIEvent.MouseEvent (buttons, fromEvent)

mkGol :: Component { world::World, size::CanvasSize }
mkGol = do
  ui <- mkUI
  component "Gol" \props -> React.do
    world /\ setWorld <- useState props.world
    worldGrid /\ setWorldGrid <- useState (Nothing :: Maybe WorldGrid)
    modifyType /\ setModifyType <- useState Alive
    running /\ setRunning <- useState true
    fr /\ setFr <- useState 30
    canvas <- useRef null
  
    useEffect world do
      current <- readRefMaybe canvas
      case current of
        Nothing -> pure mempty
        Just node -> do
          grid <- renderWorld (nodeToCanvasElement node) world
          setWorldGrid $ \_ -> Just grid
          pure mempty

    useEffect { running, fr } $
      if running then do
        intervalId <- setInterval (toInterval fr) $ setWorld (\w -> tick w)
        pure $ clearInterval intervalId
      else pure mempty
          
    let modifyWorld nativeEvent =
          let mouseEvent = (unsafePartial fromJust) $ fromEvent nativeEvent
          in if buttons mouseEvent `mod` 2 == 0
             then pure unit
             else do
               let clickY = toNumber $ offsetY mouseEvent
                   clickX = toNumber $ offsetX mouseEvent
                   { row, col } = unsafePointToCell worldGrid clickY clickX
               case setCell modifyType world { r:row, c:col } of
                 Nothing -> pure unit
                 Just world' -> setWorld \_ -> world'
            
    let beginModify nativeEvent = do
          let mouseEvent = (unsafePartial fromJust) $ fromEvent nativeEvent
              clickY = toNumber $ offsetY mouseEvent
              clickX = toNumber $ offsetX mouseEvent
              { row, col } = unsafePointToCell worldGrid clickY clickX
          case getCell world { c:col, r:row } of
            Nothing -> pure unit
            Just cell -> setModifyType ( \_ -> toggle cell ) *>
                         modifyWorld nativeEvent

    pure $
      D.div { id:"container"
            , children:
              [ D.canvas { ref:canvas
                         , id:"gol"
                         , width:props.size.width
                         , height:props.size.height
                         , onMouseDown: capture nativeEvent beginModify
                         , onMouseMove: capture nativeEvent modifyWorld
                         }
              , ui { world, setWorld, running, setRunning, fr, setFr }
              ]
            }

    
