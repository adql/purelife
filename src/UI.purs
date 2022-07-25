module UI where

import Prelude

import Data.Array ((..))
import Data.Int (floor, toNumber)
import Data.Traversable (traverse_)
import Effect (Effect)
import Graphics.Canvas (CanvasElement, Context2D)
import Graphics.Canvas as C

style = { gridColor: "#CCC"
        , gridLineWidth: 0.3
        }

type WorldGrid = { ctx::Context2D
                 , w::Number
                 , h::Number
                 , cols::Int
                 }

type Line = { x0::Number
            , y0::Number
            , x::Number
            , y::Number
            }

cellSize :: Number -> Int -> Number
cellSize w cols = (w - 1.0) / toNumber cols

gridRows :: Number -> Number -> Int
gridRows h size = floor $ h / size

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
drawGrid { ctx, w, h, cols } = do
  C.setStrokeStyle ctx style.gridColor
  C.setLineWidth ctx style.gridLineWidth
  let size = cellSize w cols
      rows = gridRows h size
      xLines = map (\pos -> toLine (size * toNumber pos) h false) (0 .. cols)
      yLines = map (\pos -> toLine (size * toNumber pos) w true) (0 .. rows)
  
  traverse_ (drawLine ctx) xLines
  traverse_ (drawLine ctx) yLines

mkWorldGrid :: CanvasElement -> Int -> Effect WorldGrid
mkWorldGrid canvas cols = do
  w <- C.getCanvasWidth canvas
  h <- C.getCanvasHeight canvas
  ctx <- C.getContext2D canvas

  pure { ctx, w, h, cols }
