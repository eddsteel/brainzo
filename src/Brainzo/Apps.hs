{-# LANGUAGE OverloadedStrings #-}
module Brainzo.Apps(browser,mplayer,simulateKey) where

import Data.Maybe(fromMaybe, isNothing)
import Turtle

browser    :: Text -> Shell ()
browser url = do
  _ <- inproc "xdg-open" [url] empty -- this obviously sucks
  return ()

-- run mplayer
mplayerOptions :: [Text]
mplayerOptions = ["-prefer-ipv4", "-ao", "alsa"]

mplayer :: Text -> Shell Text
mplayer url = inproc "mplayer" (url:mplayerOptions) empty

simulateKey :: Text -> Shell ()
simulateKey k = do
  -- quick soln to using this from ssh (i.e. remote control)
  disp <- need "DISPLAY"
  let dsp = fromMaybe ":0" disp
  _ <- when (isNothing disp) (export "DISPLAY" dsp)
  inproc "xdotool" ("key":k:[]) empty >> return ()
