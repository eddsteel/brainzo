{-# LANGUAGE OverloadedStrings #-}
module Brainzo.Keys(key,xdotool) where

import Data.Maybe(fromMaybe)
import Data.Map(Map, fromList, findWithDefault)
import Data.Text (Text)
import Turtle

type Key = Text

aliases :: Map Text Key
aliases = fromList [("louder", "XF86AudioRaiseVolume")
                   , ("quieter", "XF86AudioLowerVolume")
                   , ("pause", "space")
                   , ("play", "space")
                   , ("rwd", "ctrl+Left")
                   , ("fwd", "ctrl+Right")
                   , ("sleep", "XF86Sleep")]

key :: [Text] -> (Shell (), [Text])
key (k:rest) = (xdotool key, rest)
  where key = findWithDefault k k aliases
key [] = (err usage, [])

usage :: Text
usage = "key <key name or alias>"

xdotool :: Key -> Shell ()
xdotool k = do
  -- quick soln to using this from ssh (i.e. remote control)
  disp <- need "DISPLAY"
  let dsp = fromMaybe ":0" disp
  _ <- case disp of
         Nothing -> export "DISPLAY" dsp
         _ -> return ()
  inproc "xdotool" ("key":k:[]) empty >> return ()
