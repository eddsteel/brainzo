module Main where

import Brainzo
import Data.List.NonEmpty(NonEmpty)
import qualified Data.List.NonEmpty as NEL
import Turtle (sh, arguments, err, echo, Line, Shell)

main :: IO ()
main = sh $ do
  brainzo <- birth
  a <- arguments
  if null a
    then traverse err brainzoUsage
    else
    let work :: Shell (Maybe (NonEmpty Line))
        work = fmap NEL.nonEmpty $ getToWork brainzo a
    in
      work >>= \w -> case w of
        Just q -> traverse echo q
        Nothing -> return (NEL.fromList [()])
