module Main where

import qualified Brainzo
import System.Environment

main :: IO ()
main = do
  e <- Brainzo.loadEnv
  getArgs >>= Brainzo.dispatch e
