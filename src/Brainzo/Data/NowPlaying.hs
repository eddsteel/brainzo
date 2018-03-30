{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Brainzo.Data.NowPlaying( NowPlaying
                              , station
                              , track
                              , playedOn
                              , fromStationTrack
                              , toStationTrack
                              , nowPlayingRadio) where

import Data.Text(Text)
import qualified Data.Text as T
import Data.Time.Clock(UTCTime)
import Database.SQLite.Simple(FromRow, fromRow, field)
import Turtle(Line, unsafeTextToLine)

data NowPlaying = NowPlayingRadio { npId :: Maybe Integer
                      , station :: Text
                      , track :: Text
                      , playedOn :: Maybe UTCTime } deriving Show

instance FromRow NowPlaying where
  fromRow = NowPlayingRadio <$> field <*> field <*> field <*> field

nowPlayingRadio        :: Maybe Integer ->  Maybe UTCTime -> Text -> Text -> NowPlaying
nowPlayingRadio i o s t = NowPlayingRadio { npId = i, station = s, track = t, playedOn = o }

fromStationTrack :: Text -> Text -> NowPlaying
fromStationTrack  = nowPlayingRadio Nothing Nothing

toStationTrack          :: NowPlaying -> Line
toStationTrack (NowPlayingRadio {..}) =
  unsafeTextToLine $ if track == ""
                     then station
                     else T.concat [station, " - ", track]
