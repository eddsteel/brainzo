module Main where

import Brainzo
import Turtle (sh, arguments)

main :: IO ()
main = sh $ do
  brainzo <- birth
  arguments >>= getToWork brainzo
