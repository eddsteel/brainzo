{-# LANGUAGE OverloadedStrings #-}
module Brainzo.Commands.Audio(command, louder, quieter, mute, micmute, mixer) where

import Brainzo.Apps(pactl)
import Brainzo.Data
import Data.List.NonEmpty(NonEmpty((:|)))
import qualified Data.Text as T
import Text.Read(readMaybe)
import Turtle

audio :: WorkStep
audio _ args @ (a:|as) = case args of
  ("louder":|i:r)  -> (readOrBail i louder, r)
  ("quieter":|i:r) -> (readOrBail i quieter, r)
  ("mute":|r)      -> (mute, r)
  ("micmute":|r)   -> (micmute, r)
  ("mixer":|r)     -> (mixer, r)
  (op:|_)          -> (bail $ T.concat ["audio doesn't understand ", op, "."], a:as)
  where bail t = err t >> return ""
        readOrBail :: Read a => Text -> (a -> Shell Text) -> Shell Text
        readOrBail str fun = case readMaybe . T.unpack $ str of
                               Just i -> fun i
                               Nothing -> bail "invalid number"

louder :: Int -> Shell Text
louder  = adjustChannel Sink Up

quieter :: Int -> Shell Text
quieter = adjustChannel Sink Down

mute :: Shell Text
mute  = muteChannel Sink

micmute :: Shell Text
micmute  = muteChannel Source

mixer :: Shell Text
mixer  = inproc "pavucontrol" empty empty

data Channel = Source | Sink deriving Show
data Direction = Up | Down deriving Show


defaultName :: Channel -> Text
defaultName Source = "@DEFAULT_SOURCE@"
defaultName Sink = "@DEFAULT_SINK@"

adjustChannel :: Channel -> Direction -> Int -> Shell Text
adjustChannel chan dir i =
  let adj Up = "+"
      adj Down = "-"
      volCmd Source = "set-source-volume"
      volCmd Sink = "set-sink-volume"
  in pactl [volCmd chan, defaultName chan, T.concat [adj dir, T.pack (show i), "%"]]

muteChannel  :: Channel -> Shell Text
muteChannel c =
  let muteCmd Source = "set-source-mute"
      muteCmd Sink = "set-sink-mute"
  in pactl [muteCmd c, defaultName c, "toggle"]

command :: Command
command  = Cmd "audio" ["louder", "quieter", "mute", "micmute", "mixer"] audio []
