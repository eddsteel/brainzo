{-# LANGUAGE OverloadedStrings #-}
module Brainzo.Apps(browser,mplayer,simulateKey,pactl,pamixer) where

import Brainzo.Data(Lines)
import Data.Maybe(fromMaybe, isNothing)
import Turtle

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
