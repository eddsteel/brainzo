{-# LANGUAGE OverloadedStrings #-}
module Brainzo.Notify(notifyPipe) where
import Brainzo.Util
import Control.Exception
import qualified Data.Text as T
import DBus.Client(ClientError, clientError)
import DBus.Notify
import DBus.Notify(Icon)
import Turtle(Text, Shell, liftIO)

-- Pass through text, sending over DBUS to notify, ignoring errors
--
notifyPipe :: Text -> Text -> Shell Text
notifyPipe i t = liftIO $ do
  client <- try connectSession :: IO (Either ClientError Client)
  let note = blankNote { summary = T.unpack t, appName = "Brainzo", appImage = Just (Icon \./ i)}
  _ <- try $ safenotify client note :: IO (Either ClientError Notification)
  return t
  where
    safenotify (Right c) n = notify c n
    safenotify (Left _) _ = return $ throw $ clientError "no client"
