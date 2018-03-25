{-# LANGUAGE OverloadedStrings #-}
module Brainzo.Apps(browser,mplayer,simulateKey,pactl,pamixer) where

import Data.Maybe(fromMaybe, isNothing)
import Turtle

browser    :: Text -> Shell Text
browser url = inproc "xdg-open" [url] empty

-- run mplayer
mplayerOptions :: [Text]
mplayerOptions = ["-prefer-ipv4", "-ao", "alsa"]

mplayer :: Text -> Shell Text
mplayer url = inproc "mplayer" (url:mplayerOptions) empty

pactl :: [Text] -> Shell Text
pactl args = inproc "pactl" args empty

pamixer :: Shell Text
pamixer = inproc "pavucontrol" empty empty

simulateKey :: Text -> Shell Text
simulateKey k = do
  -- quick soln to using this from ssh (i.e. remote control)
  disp <- need "DISPLAY"
  let dsp = fromMaybe ":0" disp
  _ <- when (isNothing disp) (export "DISPLAY" dsp)
  inproc "xdotool" ("key":k:[]) empty
