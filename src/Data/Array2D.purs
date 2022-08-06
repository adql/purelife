module Data.Array2D where

import Prelude

import Data.Array (index, length, zipWith, (!!), (..))
import Data.Array as A
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.Maybe (Maybe(..))
import Data.Traversable (class Traversable, sequence, traverseDefault)

newtype Array2D a = Array2D (Array (Array a))

derive instance eqArray2D :: Eq a => Eq (Array2D a)

derive newtype instance showArray2D :: Show a => Show (Array2D a)

derive instance functorArray2D :: Functor Array2D

instance foldableArray2D :: Foldable Array2D where
  foldMap f (Array2D xss) = foldMap f $ A.concat xss
  foldl f acc arr = foldlDefault f acc arr
  foldr f acc arr = foldrDefault f acc arr

instance traversableArray2D :: Traversable Array2D where
  sequence (Array2D xss) = map Array2D $ sequence $ map sequence xss
  traverse f arr = traverseDefault f arr

type Index2D = { r::Int, c::Int }

index2D :: forall a. Array2D a -> Index2D -> Maybe a
index2D (Array2D xss) {r,c} = index xss r >>= \row -> index row c

modifyAt2D :: forall a. Index2D -> (a -> a) -> Array2D a -> Maybe (Array2D a)
modifyAt2D {r,c} f (Array2D xss) = do
  newRow <- A.modifyAt c f =<< index xss r
  newXss <- A.updateAt r newRow xss
  pure $ Array2D newXss

dimensions :: forall a. Array2D a -> Record ( rows :: Int, cols :: Int )
dimensions (Array2D xss) = { rows, cols }
  where
    rows = length xss
    cols = case xss!!0 of
      Nothing  -> 0
      Just row -> length row

mapWithIndex2D :: forall a b. (Index2D -> a -> b) -> Array2D a -> Array2D b
mapWithIndex2D f arr@(Array2D xss) = Array2D $ zipWith g (0 .. (rows - 1)) xss
  where
    { rows, cols } = dimensions arr
    g row xs = zipWith (\col x -> f { r:row, c:col } x) (0 .. (cols - 1)) xs

replicate2D :: forall a. Int -> Int -> a -> Array2D a
replicate2D rows cols x = Array2D $ A.replicate rows $ A.replicate cols x
