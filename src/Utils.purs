module Utils where

import Graphics.Canvas (CanvasElement)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (Node)

nodeToCanvasElement :: Node -> CanvasElement
nodeToCanvasElement = unsafeCoerce
