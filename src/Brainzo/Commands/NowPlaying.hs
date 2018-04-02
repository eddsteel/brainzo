{-# LANGUAGE OverloadedStrings #-}
module Brainzo.Commands.NowPlaying(command, retrieveNP) where

import Brainzo.Apps(consulValue)
import Brainzo.Data
import Brainzo.Data.NowPlaying
import Brainzo.Notify
import Data.Aeson
import Data.List.NonEmpty(NonEmpty((:|)))
import Data.Maybe(fromMaybe)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Text as T
import Turtle

nowPlaying :: WorkStep
nowPlaying _ ("get":|r) = (retrieveNP, r)
nowPlaying _ ("display":|r) = (displayNP, r)
nowPlaying _ as@(op:|_) = (bail $ T.concat ["audio doesn't understand ", op, "."], NEL.toList as)
  where bail t = err (unsafeTextToLine t) >> return mempty

retrieveNP :: Shell [Line]
retrieveNP = do
  consulJSON <- consulValue "now-playing"
  let np = decodeStrict consulJSON :: Maybe NowPlaying
  let result = fromMaybe [] $ (:[]) . toLine <$> np
  return result

displayNP :: Shell [Line]
displayNP = do
  result <- retrieveNP
  _ <- notifyPipe icon . head $ result
  return result

icon :: Line
icon = unsafeTextToLine "media-playback-start"

command :: Command
command  = Cmd "np" ["display", "get"] nowPlaying []
