module Main where

import Brainzo
import Turtle (sh, arguments, err)

main :: IO ()
main = sh $ do
  brainzo <- birth
  a <- arguments
  if null a then err brainzoUsage
  else getToWork brainzo a
