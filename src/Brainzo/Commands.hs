{-# LANGUAGE OverloadedStrings #-}

module Brainzo.Commands where

import Brainzo.Data
import qualified Brainzo.Commands.Radio as R
import qualified Brainzo.Commands.GoogleMaps as GM
import qualified Brainzo.Commands.Keys as K
import Data.List(intersperse)
import qualified Data.Text as T
import Data.Map.Strict(Map, fromList)
import Turtle

usage :: Command -> Text
usage (Cmd cmd subs _ _) = T.snoc (T.concat $ cmd : " [" : (intersperse "|" subs)) ']'

radio :: Command
radio = R.command

keys :: Command
keys = K.command

googleMaps :: Command
googleMaps = GM.command

commands :: Map Text Command
commands = fromList [ ("radio", radio)
                    , ("map",   googleMaps)
                    , ("key",   keys)]
