{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Brainzo.Data.NowPlaying( NowPlaying
                              , station
                              , track
                              , playedOn
                              , isfresh
                              , fromStationTrack
                              , stamped
                              , toLine
                              , nowTrack
                              , nowFilm
                              , nowPlayingRadio) where

import Data.Aeson
import qualified Data.HashMap.Lazy as HML(keys)
import Data.Text(Text)
import qualified Data.Text as T
import Data.Maybe(maybe)
import Data.Time
import Data.Time.Clock(UTCTime)
import Data.Time.Clock.POSIX
import Database.SQLite.Simple(FromRow, fromRow, field)
import Turtle(Line, unsafeTextToLine)

data NowPlaying = NowPlayingRadio { npId :: Maybe Integer
                                  , station :: Text
                                  , track :: Text
                                  , playedOn :: Maybe UTCTime
                                  }
            | NowPlayingTrack { artist :: Text, title :: Text, album :: Maybe Text, time :: POSIXTime }
            | NowPlayingTelevision { series :: Text, title :: Text, time :: POSIXTime}
            | NowPlayingFilm { title :: Text, time :: POSIXTime}
            | Off
            deriving (Show, Eq)

-- we only store radio info in DB (we'll move this to consul tho)
instance FromRow NowPlaying where
  fromRow = NowPlayingRadio <$> field <*> field <*> field <*> field

nowPlayingRadio :: Maybe Integer ->  Maybe UTCTime -> Text -> Text -> NowPlaying
nowPlayingRadio i o s t = NowPlayingRadio { npId = i, station = s, track = t, playedOn = o }

fromStationTrack :: Text -> Text -> NowPlaying
fromStationTrack  = nowPlayingRadio Nothing Nothing

nowTrack :: Text -> Text -> IO NowPlaying
nowTrack artist title = NowPlayingTrack artist title Nothing `fmap` (fmap utcTimeToPOSIXSeconds getCurrentTime)

nowFilm :: Text -> IO NowPlaying
nowFilm title = NowPlayingFilm title `fmap` (fmap utcTimeToPOSIXSeconds getCurrentTime)

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
    | otherwise                   = film
    where
      radio      = fromStationTrack     <$> o .: "station" <*> o .: "track"
      albumTrack = NowPlayingTrack      <$> o .: "artist"  <*> o .: "title" <*> o .: "album" <*> o .: "time"
      track      = NowPlayingTrack      <$> o .: "artist"  <*> o .: "title" <*> pure Nothing <*> o .: "time"
      episode    = NowPlayingTelevision <$> o .: "series"  <*> o .: "title" <*> o .: "time"
      film       = NowPlayingFilm       <$> o .: "title"   <*> o .: "time"
  parseJSON _ = pure Off

instance ToJSON NowPlaying where
  toJSON (NowPlayingRadio {..})      = object ["station" .= station, "track" .= track]
  toJSON (NowPlayingTrack {..})      = object ["artist" .= artist, "title" .= title, "album" .= album, "time" .= (floor time :: Integer)]
  toJSON (NowPlayingTelevision {..}) = object ["series" .= series, "title" .= title, "time" .= (floor time :: Integer)]
  toJSON (NowPlayingFilm {..})       = object ["title" .= title, "time" .= (floor time :: Integer)]
  toJSON (Off)                       = object []

isfresh                            :: Integer -> NowPlaying -> Bool
isfresh i NowPlayingRadio {..}      = (maybe 0 (floor . utcTimeToPOSIXSeconds) playedOn) - i > 300  
isfresh i NowPlayingTrack {..}      = (floor time) - i > 300
isfresh i NowPlayingTelevision {..} = (floor time) - i > 300
isfresh i NowPlayingFilm {..}       = (floor time) - i > 300
isfresh _ Off                       = False

stamped :: NowPlaying -> POSIXTime -> NowPlaying
stamped (n@NowPlayingRadio {..}) _      = n
stamped (n@NowPlayingTrack {..}) t      = n {time = t}
stamped (n@NowPlayingTelevision {..}) t = n {time = t}
stamped (n@NowPlayingFilm {..}) t       = n {time = t}
stamped (n@Off) t                       = n {time = t}
