module UI ( mkUI
          )
       where

import Prelude

import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Gol.Logic (World, emptyWorld, randomWorld, worldDimensions)
import React.Basic.DOM as D
import React.Basic.DOM.Events (capture, capture_, targetValue)
import React.Basic.Hooks (Component, component)

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
            , D.button { onClick: capture_ do
                            world' <- randomWorld (worldDimensions props.world) 0.4
                            props.setWorld \_ -> world'
                       , children: [ D.text "Random" ]
                       }
            ]}
