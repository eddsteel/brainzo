{-# LANGUAGE OverloadedStrings #-}
module Brainzo.Notify(notifyPipe, notifyAllPipe) where

import Brainzo.Util
import Control.Exception
import qualified Data.List.NonEmpty as NEL
import Data.Text(Text)
import qualified Data.Text as T
import DBus.Client(ClientError, clientError)
import DBus.Notify
import DBus.Notify(Icon(..))
import Turtle(Shell, liftIO, Line, lineToText, textToLines)
import Turtle.Shell(select)

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
    input  = (NEL.toList . textToLines) t
    output = traverse (notifyPipe i) input
  in
    output >>= \ ls -> select ls
