module Main where

import Brainzo
import Turtle (sh, arguments, select, stderr, stdout)

main :: IO ()
main = sh $ do
  a <- arguments
  if null a
    then stderr . select $ brainzoUsage
    else stdout . getToWork $ a
