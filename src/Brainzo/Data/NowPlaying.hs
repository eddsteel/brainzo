{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Brainzo.Data.NowPlaying( NowPlaying
                              , station
                              , track
                              , playedOn
                              , fromStationTrack
                              , toLine
                              , nowPlayingRadio) where

import Data.Aeson
import qualified Data.HashMap.Lazy as HML(keys)
import Data.Text(Text)
import qualified Data.Text as T
import Data.Time.Clock(UTCTime)
import Database.SQLite.Simple(FromRow, fromRow, field)
import Turtle(Line, unsafeTextToLine)


data NowPlaying = NowPlayingRadio { npId :: Maybe Integer
                      , station :: Text
                      , track :: Text
                      , playedOn :: Maybe UTCTime }
                | NowPlayingTrack { artist :: Text, title :: Text, album :: Maybe Text}
                | NowPlayingTelevision { series :: Text, title :: Text }
                | NowPlayingFilm { title :: Text}
                | Off
                deriving (Show, Eq)

-- we only store radio info in DB (we'll move this to consul tho)
instance FromRow NowPlaying where
  fromRow = NowPlayingRadio <$> field <*> field <*> field <*> field


nowPlayingRadio :: Maybe Integer ->  Maybe UTCTime -> Text -> Text -> NowPlaying
nowPlayingRadio i o s t = NowPlayingRadio { npId = i, station = s, track = t, playedOn = o }

fromStationTrack :: Text -> Text -> NowPlaying
fromStationTrack  = nowPlayingRadio Nothing Nothing

toLine :: NowPlaying -> Line
toLine = unsafeTextToLine . text
  where
    text (NowPlayingRadio {..}) = if track == "" then station else station `dash` track
    text (NowPlayingTrack {..}) = artist `dash` title `brackets` album
    text (NowPlayingTelevision {..}) = series `dash` title
    text (NowPlayingFilm {..}) = title
    text Off = "off"
    dash a b = T.concat [a, " — ", b]
    brackets a (Just b) = T.concat [a, " (", b, ")"]
    brackets a Nothing  = a

instance FromJSON NowPlaying where
  parseJSON (Object o)
    | "station" `elem` HML.keys o = radio
    | "album"   `elem` HML.keys o = albumTrack
    | "artist"  `elem` HML.keys o = track
    | "series"  `elem` HML.keys o = episode
    | otherwise                 = film
    where
      radio      = fromStationTrack     <$> o .: "station" <*> o .: "track"
      albumTrack = NowPlayingTrack      <$> o .: "artist"  <*> o .: "title" <*> o .: "album"
      track      = NowPlayingTrack      <$> o .: "artist"  <*> o .: "title" <*> pure Nothing
      episode    = NowPlayingTelevision <$> o .: "series"  <*> o .: "title"
      film       = NowPlayingFilm       <$> o .: "title"
  parseJSON _ = pure Off

instance ToJSON NowPlaying where
  toJSON (NowPlayingRadio {..})      = object ["station" .= station, "track" .= track]
  toJSON (NowPlayingTrack {..})      = object ["artist" .= artist, "title" .= title, "album" .= album]
  toJSON (NowPlayingTelevision {..}) = object ["series" .= series, "title" .= title]
  toJSON (NowPlayingFilm {..})       = object ["title" .= title]
  toJSON (Off)                       = object []
