module Main where

import Modelio

(|>) :: t1 -> (t1 -> t2) -> t2
(|>) a f = f a

main :: IO ()
main = do
    file <- Modelio.loadModel "models/sphere.ply"
    print file