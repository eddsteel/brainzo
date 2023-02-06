{-# LANGUAGE OverloadedStrings #-}

module Brainzo.Commands where

import Brainzo.Data
import qualified Brainzo.Commands.Audio as Audio
import qualified Brainzo.Commands.Blinds as Blinds
import qualified Brainzo.Commands.Radio as Radio
import qualified Brainzo.Commands.GoogleMaps as Maps
import qualified Brainzo.Commands.Keys as Keys
import qualified Brainzo.Commands.Mouse as Mouse
import qualified Brainzo.Commands.MPRIS as MPRIS
import qualified Brainzo.Commands.NowPlaying as NowPlaying
import qualified Brainzo.Commands.Version as Version
import Data.List(intersperse)
import qualified Data.Text as T
import Data.Map.Strict(Map, fromList)
import Turtle

usage :: Command -> Line
usage (Cmd cmd subs _ _) =
  unsafeTextToLine $ T.snoc (T.concat $ cmd : " [" : (intersperse "|" subs)) ']'

commands :: Map Text Command
commands = fromList . fmap cmdPair $
  [ Audio.command
  , Blinds.command
  , Keys.command
  , Maps.command
  , Mouse.command
  , MPRIS.command
  , NowPlaying.command
  , Radio.command
  , Version.command]
