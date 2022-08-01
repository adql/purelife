module Utils where

import Prelude

import Graphics.Canvas (CanvasElement)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (Node)

nodeToCanvasElement :: Node -> CanvasElement
nodeToCanvasElement = unsafeCoerce

toInterval :: Int -> Int
toInterval fr = 1000 / fr
