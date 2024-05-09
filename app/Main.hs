module Main where

import qualified Data.Vector as V

import Modelio
import Scene (ShapeData(triangles, positions, normals))

main :: IO ()
main = do
    file <- Modelio.loadShape "models/sphere.ply"
    print file