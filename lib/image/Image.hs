module Image where

import qualified Data.Vector as V

import Math (Vec2i)

data Image a = Image {
  size :: Vec2i,
  pixels :: V.Vector a
}
  deriving (Show)

consImage = Image