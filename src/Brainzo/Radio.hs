{-# LANGUAGE OverloadedStrings #-}
module Brainzo.Radio(radio, icyFormat) where

import Brainzo.DB.RadioDB
import Brainzo.Data.NowPlaying(NowPlaying, fromStationTrack, toStationTrack)
import qualified Brainzo.Data.Storage as DB
import Brainzo.File(expandHome)
import Control.Monad((=<<))
import Data.List(find)
import Data.Map(Map, toList)
import Data.Maybe(fromMaybe)
import Prelude hiding (FilePath)
import Turtle hiding (find, fromText)
import qualified Data.Map as Map
import qualified Data.Text as T

type Config    = Text
type Stations  = Map Text Text
data Direction = Bwd | Fwd

npfile         :: Shell FilePath
npfile          = expandHome ".radio-np"

orNoop         :: Maybe (Shell a) -> Shell a
orNoop          = fromMaybe empty

radio                               :: Maybe Config -> [Text] -> (Shell (), [Text])
radio (Just c) ("list":rest)         = ((list . parseStations) c, rest)
radio (Just c) ("play":station:rest) = doRadio c rest $ playByKey station
radio (Just c) ("seek":rest)         = doRadio c rest seek
radio (Just c) ("kees":rest)         = doRadio c rest kees
radio (Just c) ("on":rest)           = doRadio c rest on
radio _        ("off":rest)          = (off, rest)
radio _        ("np":rest)           = doRadio "" rest np
radio Nothing  rest                  = (err "radio needs some stations.", rest)
radio _        (op:rest)             = (err (T.append (T.append "radio doesn't understand " op) "."), rest)
radio _        []                    = (err usage, [])

doRadio :: Config -> [Text] -> (Stations -> RadioDB -> Shell()) -> (Shell (), [Text])
doRadio c rest fn = (action, rest)
  where action = do
          db <- liftIO newDB
          fn (parseStations c) db

usage          :: Text
usage           = "radio list|play <station>|on|off|seek|kees|off|np"

list           :: Stations -> Shell ()
list            = echo . T.unlines . Map.keys

np             :: Stations -> RadioDB -> Shell ()
np _ db         = withNP nowPlaying (echo "off")
  where nowPlaying :: Text -> Shell ()
        nowPlaying _ = liftIO $ fmap toStationTrack (DB.retrieve db) >>= echo

seek           :: Stations -> RadioDB -> Shell ()
seek c db       = withNP (playNext Fwd c db) (playFirst c db)

kees           :: Stations -> RadioDB -> Shell ()
kees c db       = withNP (playNext Bwd c db) (playFirst c db)

on             :: Stations -> RadioDB -> Shell ()
on c db         = withNP (playCurrent c db) (playFirst c db)

off            :: Shell ()
off             = cat [inproc "killall" ["-q", "mplayer"] empty >> endless
                      , withNP (\_ -> npfile >>= rm ) empty ]

play           :: RadioDB -> Text -> Text -> Shell ()
play db key url = cat [off
                      , npfile >>= \file -> output file (return key)
                      -- play radio, piping NP to store. Start with empty string
                      -- so that we get an entry for stations that don't publish
                      -- now playing info.
                      , stdout $ store . fromStationTrack key =<< cat [pure "", mplayer url]]
  where store  :: NowPlaying -> Shell Text
        store n = liftIO (DB.store n db)

mplayerOptions :: [Text]
mplayerOptions = ["-prefer-ipv4", "-ao", "alsa"]

icyPrefix :: Pattern Text
icyPrefix = "ICY Info: StreamTitle"

-- ICY Info: StreamTitle='Angus & Julia Stone - Old Friend';StreamUrl='';
icyFormat :: Pattern Text
icyFormat = do
  choice
    [ has $ between (icyPrefix <> "='") ("';StreamUrl='" <> chars) chars
    , has $ between (icyPrefix <> "='") "';" chars] -- :(

-- run mplayer
mplayer        :: Text -> Shell Text
mplayer url     =
  let
    mplayerO  = inproc "mplayer" (url:mplayerOptions) empty
    filtered  = grep (prefix icyPrefix) mplayerO
  in
    sed icyFormat filtered

playByKey        :: Text -> Stations -> RadioDB -> Shell ()
playByKey s ss db = orNoop (play db s <$> Map.lookup s ss)

playNext            :: Direction -> Stations -> RadioDB -> Text -> Shell ()
playNext d c db key  = uncurry (play db) (next c)
  where
    streams Fwd = Prelude.concat . repeat . Map.toList
    streams Bwd = Prelude.concat . repeat . reverse . Map.toList
    second = head . tail
    next c = second (dropWhile ((/= key) . fst) (streams d c))

playFirst      :: Stations -> RadioDB -> Shell ()
playFirst s db  = (uncurry (play db) . (head . Map.toList)) s

playCurrent       :: Stations -> RadioDB -> Text -> Shell ()
playCurrent s db k = uncurry (play db) current
  where current = head $ filter ((== k) . fst) (toList s)

withNP         :: (Text -> Shell ()) -> Shell () -> Shell ()
withNP f g      = do
                    nf <- npfile
                    b <- testfile nf
                    if b
                      then input nf >>= f
                      else g

parseStations  :: Config -> Stations
parseStations s = Map.fromList $ foldr tupleOrDrop [] (T.words <$> T.lines s)
  where tupleOrDrop [a, b] agg = (a, b):agg
        tupleOrDrop _      agg = agg
