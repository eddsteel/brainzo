{-# LANGUAGE OverloadedStrings #-}
module Brainzo.Commands.NowPlaying(command, retrieveNP, storeNP) where

import Brainzo.Apps(consulSet, consulValue)
import Brainzo.Data
import Brainzo.Data.NowPlaying
import Brainzo.Notify
import Data.Aeson
import Data.List.NonEmpty(NonEmpty((:|)))
import Data.Maybe(maybeToList)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Text as T
import Data.Text.Encoding(decodeUtf8)
import Turtle

nowPlaying :: WorkStep
nowPlaying _ ("get":|r) = (retrieveNP, r)
nowPlaying _ ("display":|r) = (displayNP, r)
nowPlaying _ as@(op:|_) = (bail $ T.concat ["audio doesn't understand ", op, "."], NEL.toList as)
  where bail t = err (unsafeTextToLine t) >> return mempty

retrieveNP :: Shell Line
retrieveNP = do
  consulJSON <- consulValue "now-playing"
  let np = decodeStrict consulJSON :: Maybe NowPlaying
  let result = toLine <$> maybeToList np
  select result

storeNP  :: NowPlaying -> Shell Line
storeNP = (>>= select . textToLines . decodeUtf8) . consulSet "now-playing" . encode

displayNP :: Shell Line
displayNP = do
  result <- retrieveNP
  _ <- notifyPipe icon result
  return result

icon :: Line
icon = unsafeTextToLine "media-playback-start"

command :: Command
command  = Cmd "np" ["display", "get"] nowPlaying []