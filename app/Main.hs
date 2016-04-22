module Main where

import qualified Brainzo
import Turtle (sh, arguments)

main :: IO ()
main = sh $ do
  a <- arguments
  e <- Brainzo.loadEnv
  Brainzo.dispatch e a
