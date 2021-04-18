{-# LANGUAGE OverloadedStrings #-}
module Brainzo.Commands.MPRIS(command) where

import Brainzo.Apps(playerctl)
import Brainzo.Commands.NowPlaying(storeNP)
import Brainzo.Data
import Brainzo.Data.NowPlaying
import Brainzo.Util(bail)
import Data.Char(isSpace)
import Data.List.NonEmpty(NonEmpty((:|)))
import Data.Maybe(catMaybes, fromMaybe)
import qualified Data.Text as T
import Network.CGI(urlDecode)
import Turtle

mpris :: WorkStep
mpris args = case args of
  ("set-np":|_) -> setNP
  ("play-pause":|_) -> playPause
  ("previous":|_) -> previous
  ("back":|_) -> back
  ("forward":|_) -> forward
  ("next":|_) -> next
             
  _ -> bail "mpris" args

setNP :: Shell Line
setNP = do
  titleM <- playerctl ["metadata", "title"]
  artist <- playerctl ["metadata", "artist"]
  fileurl <- playerctl ["metadata", "xesam:url"]
  fileio <- trim . fromMaybe "" $ fileurl
  let file = T.pack . urlDecode . T.unpack . lineToText $ fileio
  np <- liftIO . nowFilm . fromMaybe file $ titleM
  storeNP np

status :: Shell Text
status = fromMaybe "" <$> playerctl ["status"]

playPause :: Shell Line
playPause = do
  _ <- playerctl ["play-pause"]
  isPlaying <- T.filter (not . isSpace) <$> status
  if isPlaying == "Playing" then setNP else return $ unsafeTextToLine isPlaying

previous :: Shell Line
previous = do
  _ <- playerctl ["previous"]
  setNP

next :: Shell Line
next = do
  _ <- playerctl ["next"]
  setNP

back :: Shell Line
back = playerctl ["position", "10-"] >> empty
  
forward :: Shell Line
forward = playerctl ["position", "30+"] >> empty
  
trim :: Text -> Shell Line
trim t = sed p (return $ unsafeTextToLine t)
  where p = (star anyChar) *> char '/' *>
          (star (notChar '/'))
          <* char '.' <* (star (notChar '.'))

command :: Command
command  = Cmd "mpris" [ "set-np"
                       , "play-pause"
                       , "previous"
                       , "back"
                       , "forward"
                       , "next"]
           mpris []
