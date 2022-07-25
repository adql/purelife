module Gol.Render where

import Prelude

import Data.Array ((..))
import Data.Int (floor, toNumber)
import Data.Traversable (traverse_)
import Effect (Effect)
import Graphics.Canvas (CanvasElement, Context2D)
import Graphics.Canvas as C

style = { gridColor: "#CCC"
        , gridLineWidth: 0.3
        , background: "#FFF"
        , cellColor: "#555"
        }

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

toLine :: Number -> Number -> Boolean -> Line
toLine pos len hor =
  let x0 = if hor then 0.0 else pos
      y0 = if hor then pos else 0.0
      x = if hor then len else pos
      y = if hor then pos else len
  in
    { x0, y0, x, y }

drawLine :: Context2D -> Line -> Effect Unit
drawLine ctx line = C.strokePath ctx $ do
  C.moveTo ctx line.x0 line.y0
  C.lineTo ctx line.x line.y
  C.closePath ctx

drawGrid :: WorldGrid -> Effect Unit
drawGrid { ctx, w, h, cols, rows, cellSize } = do
  C.setStrokeStyle ctx style.gridColor
  C.setLineWidth ctx style.gridLineWidth
  let xLines = map (\pos -> toLine (cellSize * toNumber pos) h false) (0 .. cols)
      yLines = map (\pos -> toLine (cellSize * toNumber pos) w true) (0 .. rows)
  
  C.setFillStyle ctx style.background
  C.fillRect ctx { x:0.0, y:0.0, width:w, height:h }
  traverse_ (drawLine ctx) xLines
  traverse_ (drawLine ctx) yLines

mkWorldGrid :: CanvasElement -> Int -> Effect WorldGrid
mkWorldGrid canvas cols = do
  w <- C.getCanvasWidth canvas
  h <- C.getCanvasHeight canvas
  ctx <- C.getContext2D canvas
  let cellSize = (w - 1.0) / toNumber cols
      rows = floor $ h / cellSize

  pure { ctx, w, h, cols, rows, cellSize}

drawCell :: WorldGrid -> Int -> Int -> Boolean -> Effect Unit
drawCell { ctx, w, h, cols, rows, cellSize } col row alive =
  if col >= cols || row >= rows then
    pure unit
  else let x = (toNumber col) * cellSize + 1.0
           y = (toNumber row) * cellSize + 1.0
           size = cellSize - 2.0
           c = if alive then style.cellColor else style.background
       in do
         C.setFillStyle ctx c
         C.fillRect ctx { x, y, width:size, height:size }
