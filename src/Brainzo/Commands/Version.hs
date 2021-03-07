{-# LANGUAGE OverloadedStrings #-}
module Brainzo.Commands.Version(command) where

import Brainzo.Data
import Turtle
import Paths_brainzo (version)
import Data.Version (showVersion)

import qualified Data.Text as T
import qualified Data.List.NonEmpty as NEL

printVersion :: WorkStep
printVersion _ = echo v >> empty
  where v = NEL.head . textToLines . T.pack . showVersion $ version

command :: Command
command = Cmd "version" [] printVersion []
