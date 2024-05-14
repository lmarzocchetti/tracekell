module Main where

import Modelio

main :: IO ()
main = do
    file <- Modelio.loadPlyScene "models/sphere.ply"
    -- TODO: Convert To Raytrace Materials
    print file