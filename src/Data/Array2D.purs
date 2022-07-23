module Data.Array2D where

import Prelude

import Data.Array (index, length, zipWith, (!!), (..))
import Data.Maybe (Maybe(..))

type Array2D a = Array (Array a)

type Index2D = { r::Int, c::Int }

index2D :: forall a. Array2D a -> Index2D -> Maybe a
index2D xss {r,c} = index xss r >>= \row -> index row c

dimensions :: forall a. Array2D a -> Record ( rows :: Int, cols :: Int )
dimensions xss = { rows, cols }
  where
    rows = length xss
    cols = case xss!!0 of
      Nothing  -> 0
      Just row -> length row

mapWithIndex2D :: forall a b. (Index2D -> a -> b) -> Array2D a -> Array2D b
mapWithIndex2D f xss = zipWith g (0 .. (rows - 1)) xss
  where
    { rows, cols } = dimensions xss
    g row xs = zipWith (\col x -> f { r:row, c:col } x) (0 .. (cols - 1)) xs
