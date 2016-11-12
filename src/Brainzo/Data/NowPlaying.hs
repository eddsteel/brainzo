{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Brainzo.Data.NowPlaying( NowPlaying
                              , station
                              , track
                              , playedOn
                              , fromStationTrack
                              , toStationTrack
                              , np) where

import Data.Text(Text)
import qualified Data.Text as T
import Data.Time.Clock(UTCTime)
import Database.SQLite.Simple(FromRow, fromRow, field)

data NowPlaying = NP { npId :: Maybe Integer
                     , station :: Text
                     , track :: Text
                     , playedOn :: Maybe UTCTime } deriving Show

instance FromRow NowPlaying where
  fromRow = NP <$> field <*> field <*> field <*> field

np        :: Maybe Integer ->  Maybe UTCTime -> Text -> Text -> NowPlaying
np i o s t = NP { npId = i, station = s, track = t, playedOn = o }

fromStationTrack :: Text -> Text -> NowPlaying
fromStationTrack  = np Nothing Nothing

toStationTrack          :: NowPlaying -> Text
toStationTrack (NP {..}) = if track == ""
                           then station
                           else T.concat [station, " - ", track]
