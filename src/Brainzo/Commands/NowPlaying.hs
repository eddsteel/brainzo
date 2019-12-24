{-# LANGUAGE OverloadedStrings #-}
module Brainzo.Commands.NowPlaying(command, retrieveNP, storeNP) where

import Brainzo.Apps(consulSet, consulValue)
import Brainzo.Data
import Brainzo.Data.NowPlaying
import Brainzo.Notify
import Data.Aeson
import Data.List.NonEmpty(NonEmpty((:|)))
import Data.Maybe(maybeToList, fromMaybe)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Text as T
import Data.Text.Encoding(encodeUtf8,decodeUtf8)
import Turtle

nowPlaying :: WorkStep
nowPlaying _ ("get":|r) = (retrieveNP, r)
nowPlaying _ ("display":|r) = (displayNP, r)
nowPlaying _ ("set":|json:r) = (parseAndSetNP json, r)
nowPlaying _ as@(op:|_) = (bail $ T.concat ["np doesn't understand ", op, "."], NEL.toList as)
  where bail t = err (unsafeTextToLine t) >> return mempty

retrieveNP :: Shell Line
retrieveNP = do
  consulJSON <- consulValue "now-playing"
  let np = decodeStrict consulJSON :: Maybe NowPlaying
  let result = toLine <$> maybeToList np
  select result

parseAndSetNP :: Text -> Shell Line
parseAndSetNP json =
  let np = decodeStrict (encodeUtf8 json) :: Maybe NowPlaying
  in fromMaybe empty $ storeNP <$> np

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
command  = Cmd "np" ["display", "get", "set <json>"] nowPlaying []
