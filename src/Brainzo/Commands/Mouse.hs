{-# LANGUAGE OverloadedStrings #-}
module Brainzo.Commands.Mouse(command) where

import Brainzo.Apps(mouseMove, mouseClick)
import Brainzo.Data
import Brainzo.Data.Mouse(readDirection)
import Data.List.NonEmpty(NonEmpty((:|)))
import Turtle(err)

mouse :: WorkStep
mouse _ ("click":|rest) = (mouseClick, rest)
mouse _ (dir:|rest) = let
  mdir = readDirection dir
  in case mdir of
    Just d -> (mouseMove d 20, rest)
    Nothing -> (err "bad direction" >> mempty, rest)

command :: Command
command = Cmd "mouse" ["<direction or 'click'>"] mouse []
