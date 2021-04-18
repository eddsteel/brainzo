{-# LANGUAGE OverloadedStrings #-}
module Brainzo.Apps(browser,mplayer,mouseMove,mouseClick,keyPress,killall,pactl,pamixer,consulValue,consulSet,playerctl) where

-- | Apps called via shell that should be replaced with libraries
-- (and then they should move to (processes))
import Data.Maybe(fromMaybe, isNothing)
import qualified Data.Text as T
import Turtle
import Brainzo.Data.Mouse(MouseDirection, deltaT)
import System.IO.Unsafe(unsafePerformIO)

browser    :: Text -> Shell Line
browser url = inproc "xdg-open" [url] empty

dotool :: Text
dotool = "xdotool"

-- run mplayer
mplayerOptions :: [Text]
mplayerOptions = ["-prefer-ipv4", "-ao", "alsa"]

mplayer :: Line -> Shell Line
mplayer url = inproc "mplayer" (lineToText url:mplayerOptions) empty

pactl :: [Text] -> Shell Line
pactl args = inproc "pactl" args empty

pamixer :: Shell Line
pamixer = inproc "pavucontrol" empty empty

playerctl :: [Text] -> Shell (Maybe Text)
playerctl args = wrap <$> procStrict "playerctl" args empty
  where wrap (ExitSuccess, t) = Just t
        wrap _                = Nothing

keyPress :: Text -> Shell Line
keyPress k = do
  -- quick soln to using this from ssh (i.e. remote control)
  disp <- need "DISPLAY"
  let dsp = fromMaybe ":0" disp
  _ <- when (isNothing disp) (export "DISPLAY" dsp)
  inproc dotool ["key", k] empty

mouseClick :: Shell Line
mouseClick = inproc dotool ["click", "1"] empty

mouseMove :: MouseDirection -> Int -> Shell Line
mouseMove dir n = let
  xy = deltaT dir n
  in do
    disp <- need "DISPLAY"
    let dsp = fromMaybe ":0" disp
    _ <- when (isNothing disp) (export "DISPLAY" dsp)
    let _ = unsafePerformIO $ putStrLn (concat (fmap T.unpack xy))
    inproc dotool ("mousemove_relative" : "--" : xy) empty

consulValue :: Text -> Shell Line
consulValue k = inproc "consul" ["kv", "get", k] empty

consulSet :: Text -> Text -> Shell Line
consulSet k v = inproc "consul" ["kv", "put", k, v] empty

killall :: Text -> Shell Line
killall name = inproc "killall" ["-q", name] empty
