{-# LANGUAGE OverloadedStrings #-}

module Brainzo.Commands where

import Brainzo.Data
import qualified Brainzo.Commands.Audio as Audio
import qualified Brainzo.Commands.Radio as Radio
import qualified Brainzo.Commands.GoogleMaps as Maps
import qualified Brainzo.Commands.Keys as Keys
import qualified Brainzo.Commands.Mouse as Mouse
import qualified Brainzo.Commands.NowPlaying as NowPlaying
import qualified Brainzo.Commands.Version as Version
import Data.List(intersperse)
import qualified Data.Text as T
import Data.Map.Strict(Map, fromList)
import Turtle

usage :: Command -> Line
usage (Cmd cmd subs _ _) = unsafeTextToLine $ T.snoc (T.concat $ cmd : " [" : (intersperse "|" subs)) ']'

commands :: Map Text Command
commands = fromList [ ("audio", Audio.command)
                    , ("key",   Keys.command)
                    , ("map",   Maps.command)
                    , ("mouse", Mouse.command)
                    , ("np",    NowPlaying.command)
                    , ("radio", Radio.command)
                    , ("version", Version.command)]
