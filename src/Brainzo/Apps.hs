{-# LANGUAGE OverloadedStrings #-}
module Brainzo.Apps(browser,mplayer,mouseMove,mouseClick,keyPress,pactl,pamixer,consulValue,consulSet) where

-- | Apps called via shell that should be replaced with libraries.
import Data.ByteString(ByteString)
import Data.Maybe(fromMaybe, isNothing)
import qualified Data.Text as T
import Turtle
import qualified Turtle.Bytes as BS
import Data.Text.Lazy(toStrict)
import Data.Text.Lazy.Encoding
import qualified Data.ByteString.Lazy as LBS
import Brainzo.Data.Mouse(MouseDirection, deltaT)
import System.IO.Unsafe(unsafePerformIO)

browser    :: Text -> Shell Line
browser url = inproc "xdg-open" [url] empty

-- run mplayer
mplayerOptions :: [Text]
mplayerOptions = ["-prefer-ipv4", "-ao", "alsa"]

mplayer :: Line -> Shell Line
mplayer url = inproc "mplayer" (lineToText url:mplayerOptions) empty

pactl :: [Text] -> Shell Line
pactl args = inproc "pactl" args empty

pamixer :: Shell Line
pamixer = inproc "pavucontrol" empty empty

keyPress :: Text -> Shell Line
keyPress k = do
  -- quick soln to using this from ssh (i.e. remote control)
  disp <- need "DISPLAY"
  let dsp = fromMaybe ":0" disp
  _ <- when (isNothing disp) (export "DISPLAY" dsp)
  inproc "xdotool" ["key", k] empty

mouseClick :: Shell Line
mouseClick = inproc "xdotool" ["click", "1"] empty

mouseMove :: MouseDirection -> Int -> Shell Line
mouseMove dir n = let
  xy = deltaT dir n
  in do
    disp <- need "DISPLAY"
    let dsp = fromMaybe ":0" disp
    _ <- when (isNothing disp) (export "DISPLAY" dsp)
    let _ = unsafePerformIO $ putStrLn (concat (fmap T.unpack xy))
    inproc "xdotool" (["mousemove_relative", "--"] `mappend` xy) empty

consulValue :: Text -> Shell ByteString
consulValue k = BS.inproc "consul" ["kv", "get", k] empty

consulSet :: LBS.ByteString -> LBS.ByteString -> Shell ByteString
consulSet k v = BS.inproc "consul" ["kv", "put", force k, force v] empty
  where force = toStrict . decodeUtf8
