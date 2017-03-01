{-# LANGUAGE OverloadedStrings #-}
module Brainzo.Commands.Keys(command) where

import Brainzo.Apps(simulateKey)
import Brainzo.Data
import Brainzo.Notify(notifyPipe)
import Data.Text(Text)
import qualified Data.Text as T
import Data.List.NonEmpty(NonEmpty((:|)))
import Data.Map.Strict(Map, fromList, findWithDefault)

type KeySym = Text

icon :: Text
icon = "preferences-desktop-keyboard"

aliases :: Map Text KeySym
aliases = fromList [ ("louder", "XF86AudioRaiseVolume")
                   , ("quieter", "XF86AudioLowerVolume")
                   , ("pause", "space")   -- because for active players this is
                   , ("play", "space")    -- more reliable than XF86*
                   , ("rwd", "ctrl+Left") -- and here, too
                   , ("fwd", "ctrl+Right")--
                   , ("close", "super+w")
                   , ("sleep", "XF86Sleep")]

key :: WorkStep
key _ (k:|rest) = (notifyPipe icon msg >> simulateKey keySym, rest)
  where
    msg = T.concat ["Keypress ", k]
    keySym = findWithDefault k k aliases

command :: Command
command = Cmd "key" ["<key name or alias>"] key []