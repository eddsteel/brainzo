{-# LANGUAGE OverloadedStrings #-}
module Brainzo.Keys(key,xdotool) where

import Data.Map(Map, fromList, findWithDefault)
import Data.Text hiding (empty)
import Turtle

type Key = Text

aliases :: Map Text Key
aliases = fromList [("louder", "XF86AudioRaiseVolume"), ("quieter", "XF86AudioLowerVolume")]

key :: [Text] -> (Shell (), [Text])
key (k:rest) = (xdotool [k, keyName], rest)
  where keyName = findWithDefault k k aliases
key [] = (err usage, [])

usage :: Text
usage = "key <key name or alias>"

xdotool :: [Text] -> Shell ()
xdotool args =
  do
  _ <- inproc "xdotool" ("key":args) empty
  return ()
