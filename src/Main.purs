module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (error)
import Gol (mkGol)
import Gol.Canvas (CanvasSize)
import Gol.Logic (randomWorld)
import React.Basic.DOM as D
import React.Basic.DOM.Client (createRoot, renderRoot)
import React.Basic.Hooks (fragment)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

canvasSize :: CanvasSize
canvasSize = { width:"600", height:"600" }

main :: Effect Unit
main = do
  doc <- document =<< window
  div <- getElementById "app" $ toNonElementParentNode doc
  case div of
    Nothing -> error "No canvas found"
    Just app -> do
      root <- createRoot app
      world <- randomWorld { rows: 50, cols: 50 } 0.4
      gol <- mkGol
      renderRoot root $
        fragment [ D.header_ [ D.h1_ [ D.text "Purelife" ] ]
                 , D.main_ [ gol { world, size:canvasSize } ]
                 ]
