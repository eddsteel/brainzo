{-# LANGUAGE OverloadedStrings #-}
module Brainzo.Commands.GoogleMaps(command) where

import Brainzo.Apps(browser)
import Brainzo.Data
import Data.List.NonEmpty(NonEmpty)
import Data.Text(Text)
import Turtle

import qualified Data.Text as T
import qualified Data.List.NonEmpty as NEL

-- greedy unless first char is "
googleMap :: WorkStep
googleMap args = openMap args

openMap :: NonEmpty Text -> Shell Line
openMap = browser . searchURL . T.unwords . NEL.toList
  where
    searchURL = T.append "https://www.google.com/maps/search/" . T.replace " " "+"

command :: Command
command = Cmd "map" [] googleMap []
