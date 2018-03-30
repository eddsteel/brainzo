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
  where bail t = err (unsafeTextToLine t) >> return mempty
        readOrBail :: Read a => Text -> (a -> Shell [Line]) -> Shell [Line]
        readOrBail str fun = case readMaybe . T.unpack $ str of
                               Just i -> fun i
                               Nothing -> bail "invalid number"

louder :: Int -> Shell [Line]
louder  = adjustChannel Sink Up

quieter :: Int -> Shell [Line]
quieter = adjustChannel Sink Down

mute :: Shell [Line]
mute  = muteChannel Sink

micmute :: Shell [Line]
micmute  = muteChannel Source

mixer :: Shell [Line]
mixer  = (:[]) <$> inproc "pavucontrol" empty empty

data Channel = Source | Sink deriving Show
data Direction = Up | Down deriving Show


defaultName :: Channel -> Text
defaultName Source = "@DEFAULT_SOURCE@"
defaultName Sink = "@DEFAULT_SINK@"

adjustChannel :: Channel -> Direction -> Int -> Shell [Line]
adjustChannel chan dir i =
  let adj Up = "+"
      adj Down = "-"
      volCmd Source = "set-source-volume"
      volCmd Sink = "set-sink-volume"
  in pactl [volCmd chan, defaultName chan, T.concat [adj dir, T.pack (show i), "%"]]

muteChannel  :: Channel -> Shell [Line]
muteChannel c =
  let muteCmd Source = "set-source-mute"
      muteCmd Sink = "set-sink-mute"
  in pactl [muteCmd c, defaultName c, "toggle"]

command :: Command
command  = Cmd "audio" ["louder", "quieter", "mute", "micmute", "mixer"] audio []
