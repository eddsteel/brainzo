module Main where

import qualified Brainzo
import Turtle (sh, arguments)

main :: IO ()
main = sh $ do
  e <- Brainzo.loadEnv
  arguments >>= Brainzo.dispatch e
