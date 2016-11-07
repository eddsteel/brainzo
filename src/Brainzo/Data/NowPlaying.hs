{-# LANGUAGE OverloadedStrings #-}
module Brainzo.Data.NowPlaying( NowPlaying
                              , station
                              , track
                              , playedOn
                              , np
                              , fromText) where

import Data.Text
import Data.Time.Clock(UTCTime)
import Safe(headMay)
import Turtle.Pattern


data NowPlaying = NP { npId :: Maybe Integer
                     , station :: Text
                     , track :: Text
                     , playedOn :: Maybe UTCTime } deriving Show

np :: Maybe Integer -> Text -> Text -> Maybe UTCTime -> NowPlaying
np i s t o = NP { npId = i, station = s, track = t, playedOn = o }

fromText :: Text -> Maybe NowPlaying
fromText  = fmap (\(a,b) -> np Nothing a b Nothing) . headMay . match npPattern

npPattern :: Pattern (Text, Text)
npPattern = do
  a <- chars
  _ <- " - "
  t <- chars
  return (a, t)
