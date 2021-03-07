{-# LANGUAGE OverloadedStrings #-}
module Brainzo.Processes where

import Brainzo.Util
import Control.Exception
import DBus.Client(ClientError, clientError)
import DBus.Notify
import DBus.Notify(Icon(..))
import Data.Aeson
import Data.Maybe(fromMaybe)
import Data.Text(Text)
import Data.Text.Lazy(toStrict, fromStrict)
import Data.Text.Lazy.Encoding
import qualified Data.ByteString.Lazy as LBS
import Text.Regex.Applicative.Text(some, psym, match, (<|>), sym, optional)
import Turtle hiding (match)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Text as T
import qualified Text.Regex.Applicative.Text as RE

-- | Functions lifted into Shell to better interact with turtle and Apps


-- Pass through text, sending over DBUS to notify, ignoring errors
--
notifyPipe :: Line -> Line -> Shell Line
notifyPipe i t = liftIO $ do
  client <- try connectSession :: IO (Either ClientError Client)
  let sumy = T.unpack (lineToText t)
  let ico = lineToText i
  let note = blankNote { summary = sumy, appName = "Brainzo", appImage = Just (Icon \./ ico)}
  _ <- try $ safenotify client note :: IO (Either ClientError Notification)
  return t
  where
    safenotify (Right c) n = notify c n
    safenotify (Left _) _ = return $ throw $ clientError "no client"

notifyAllPipe :: Line -> Text -> Shell Line
notifyAllPipe i t =
  let
    inp  = (NEL.toList . textToLines) t
    outp = traverse (notifyPipe i) inp
  in
    outp >>= \ els -> select els

encodeJSON :: ToJSON a => a -> Shell Line
encodeJSON  = pure . unsafeTextToLine . toStrict . decodeUtf8 . encode

decodeJSON :: FromJSON a => Line -> Shell a
decodeJSON = fromMaybeM empty . decode . encodeUtf8 . fromStrict . lineToText

decodeKV :: FromJSON a => Line -> Shell a
decodeKV line = fromResultM empty $ toResult (parseKV (lineToText line)) >>= fromJSON

parseKV :: Text -> Maybe Value
parseKV t = object <$> kvs
  where
    kvs = match re t
    re = some kv
    kv = ((.=)) <$> key <* sym '=' <*> value <* (optional $ sym ',')
    key :: RE.RE Char Text
    key = T.pack <$> (some $ psym (/= '='))
    value :: RE.RE Char Text
    value = T.pack <$> (some $ psym (/= ','))

toResult :: Maybe a -> Result a
toResult (Just a) = Success a
toResult Nothing  = Error "empty"

fromMaybeM :: Monad m => m a -> Maybe a -> m a
fromMaybeM _ (Just a) = return a
fromMaybeM m _        = m

fromResultM :: Monad m    => m a -> Result a -> m a
fromResultM _ (Success a) = return a
fromResultM m _           = m
