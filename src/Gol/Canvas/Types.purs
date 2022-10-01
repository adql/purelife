module Gol.Canvas.Types where

import Graphics.Canvas (Context2D)

type CanvasSize = { width::String, height::String }

type WorldGrid = { ctx::Context2D
                 , w::Number
                 , h::Number
                 , cols::Int
                 , rows::Int
                 , cellSize::Number
                 }

type Line = { x0::Number
            , y0::Number
            , x::Number
            , y::Number
            }

type CanvasStyle= { gridColor :: String
                  , gridLineWidth :: Number
                  , background :: String
                  , cellColor :: String
                  }
