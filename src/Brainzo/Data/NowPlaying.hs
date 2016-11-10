{-# LANGUAGE OverloadedStrings #-}
module Brainzo.Data.NowPlaying( NowPlaying
                              , station
                              , track
                              , playedOn
                              , fromStationTrack
                              , np) where

import Data.Text
import Data.Time.Clock(UTCTime)
import Safe(headMay)
import Turtle.Pattern


data NowPlaying = NP { npId :: Maybe Integer
                     , station :: Text
                     , track :: Text
                     , playedOn :: Maybe UTCTime } deriving Show

np        :: Maybe Integer ->  Maybe UTCTime -> Text -> Text -> NowPlaying
np i o s t = NP { npId = i, station = s, track = t, playedOn = o }

fromStationTrack :: Text -> Text -> NowPlaying
fromStationTrack  = np Nothing Nothing

npPattern :: Pattern (Text, Text)
npPattern = do
  a <- chars
  _ <- " - "
  t <- chars
  return (a, t)
