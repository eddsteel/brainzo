{-# LANGUAGE OverloadedStrings #-}
module Brainzo.Apps(browser,mplayer,simulateKey,pactl,pamixer,consulValue) where

import Brainzo.Data(Lines)
import Data.ByteString(ByteString)
import Data.Maybe(fromMaybe, isNothing)
import Turtle
import qualified Turtle.Bytes as BS

browser    :: Text -> Shell Lines
browser url = (:[]) <$> inproc "xdg-open" [url] empty

-- run mplayer
mplayerOptions :: [Text]
mplayerOptions = ["-prefer-ipv4", "-ao", "alsa"]

mplayer :: Line -> Shell Line
mplayer url = inproc "mplayer" (lineToText url:mplayerOptions) empty

pactl :: [Text] -> Shell Lines
pactl args = (:[]) <$> inproc "pactl" args empty

pamixer :: Shell Line
pamixer = inproc "pavucontrol" empty empty

simulateKey :: Text -> Shell Lines
simulateKey k = do
  -- quick soln to using this from ssh (i.e. remote control)
  disp <- need "DISPLAY"
  let dsp = fromMaybe ":0" disp
  _ <- when (isNothing disp) (export "DISPLAY" dsp)
  (:[]) <$> inproc "xdotool" ("key":k:[]) empty

consulValue :: Text -> Shell ByteString
consulValue k = BS.inproc "consul" ["kv", "get", k] empty
