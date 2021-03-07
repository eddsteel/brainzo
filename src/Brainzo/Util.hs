{-# LANGUAGE OverloadedStrings #-}
module Brainzo.Util where
import Data.Text(Text, unpack)
import Data.List.NonEmpty(NonEmpty)
import Turtle(Shell, Line, err, unsafeTextToLine, empty)

import qualified Data.List.NonEmpty as NEL
import qualified Data.Text as T

(\./) :: (String -> a) -> Text -> a
(\./) f = f . unpack

bail :: Text -> NonEmpty Text -> Shell Line
bail cmd args = err (unsafeTextToLine t) >> empty
  where t = T.concat [ cmd, " doesn't understand ", T.unwords . NEL.toList $ args, "." ]
