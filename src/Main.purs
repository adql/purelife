module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (error)
import Gol (CanvasSize, mkGol, randomWorld)
import React.Basic.DOM.Client (createRoot, renderRoot)
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
      world <- randomWorld 50 50 0.4
      gol <- mkGol world canvasSize
      renderRoot root $ gol unit
