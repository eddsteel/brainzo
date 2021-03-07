{-# LANGUAGE OverloadedStrings #-}
module Brainzo.Commands.Mouse(command) where

import Brainzo.Apps(mouseMove, mouseClick)
import Brainzo.Data
import Brainzo.Data.Mouse(readDirection)
import Data.List.NonEmpty(NonEmpty((:|)))
import Turtle(err)

mouse :: WorkStep
mouse ("click":|_) = mouseClick
mouse (dir:|_) =
  let mdir = readDirection dir
  in case mdir of
    Just d -> mouseMove d 20
    Nothing -> err "bad direction" >> mempty

command :: Command
command = Cmd "mouse" ["<direction or 'click'>"] mouse []
