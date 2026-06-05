{-# LANGUAGE OverloadedStrings #-}
module Brainzo.Commands.Radio(command,icyFormat) where

import Brainzo.Apps(mplayer, killall, consulSet)
import Brainzo.DB.BrainzoDB(radioDB)
import Brainzo.DB.RadioDB
import Brainzo.Data
import Brainzo.Data.NowPlaying(NowPlaying, fromStationTrack, toLine, station)
import Brainzo.File(brainzoFile, brainzoINI)
import Brainzo.Processes(notifyPipe, notifyAllPipe, encodeJSON)
import Brainzo.Util(bail, ssvMap)
import Control.Applicative((<|>))
import Data.Bool(bool)
import Data.Either.Utils
import Data.Ini.Config
import Data.List.NonEmpty(NonEmpty((:|)))
import Data.Map.Strict(Map)
import Data.Maybe(fromMaybe, isJust, fromJust)
import Prelude hiding (FilePath)
import Turtle((<>), FilePath, Pattern, Shell, Text)
import Turtle(between, chars, choice, empty, err, grep, has, Line, lineToText, textToLines)
import Turtle(inproc, input, liftIO, output, prefix, rm, sed, select, testfile, unsafeTextToLine)
import qualified Brainzo.Data.Storage as DB
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

type Stations  = Map Text Text
data Direction = Bwd | Fwd

npfile :: Shell FilePath
npfile = brainzoFile "radio-np"

pidfile :: Shell FilePath
pidfile = brainzoFile "radio.pid"

radio :: WorkStep
radio ("list":|_)   = unsafeTextToLine <$> (stations >>= select . Map.keys)
radio ("play":|k:_) = playStation (unsafeTextToLine k)
radio ("seek":|_)   = nextStation Fwd >>= playStation
radio ("kees":|_)   = nextStation Bwd >>= playStation
radio ("on":|_)     = on
radio ("off":|_)    = off
radio ("toggle":|_) = isPlaying >>= bool on off
radio args          = bail "radio" args

on :: Shell Line
on = currentStation >>= playStation

off :: Shell Line
off = killall "mplayer" *> (npfile >>= rm) *> empty

stationsParser :: IniParser Stations
stationsParser = section "RADIO" $ do
  stns <- fieldOf "stations" string
  return $ ssvMap stns

stations :: Shell Stations
stations = do
  ini <- brainzoINI
  return . forceEither .  parseIniFile ini $ stationsParser

playStation :: Line -> Shell Line
playStation key = stationURL key >>= player >>= storeCurrent key

storeCurrent :: Line -> Line -> Shell Line
storeCurrent k o =
  let
    np = fromStationTrack (lineToText k) (lineToText o)
  in
    off
   <* (encodeJSON np >>= consulSet "now-playing" . lineToText)
   <* (npfile >>= \f -> output f (pure k))
   <* (storeDB np >>= notifyAllPipe icon)

firstStation :: Shell (Text, Text)
firstStation = head . Map.toList <$> stations

stationURL  :: Line -> Shell Line
stationURL k = unsafeTextToLine <$> url
  where url = (pure fromMaybe) <*> (snd <$> firstStation) <*> (Map.lookup (lineToText k) <$> stations)

isPlaying :: Shell Bool
isPlaying = npfile >>= testfile

currentStation :: Shell Line
currentStation = isPlaying >>= bool empty (npfile >>= input)

nextStation :: Direction -> Shell Line
nextStation dir = unsafeTextToLine <$> (isPlaying >>= bool first next)
  where
    first = fst <$> firstStation
    keys Fwd = Prelude.concat . repeat . Map.keys <$> stations
    keys Bwd = Prelude.concat . repeat . reverse . Map.keys <$> stations
    next = (lineToText <$> currentStation) >>= \ key -> head . tail . dropWhile (/= key) <$> keys dir

icyPrefix :: Pattern Text
icyPrefix = "ICY Info: StreamTitle"

-- ICY Info: StreamTitle='Angus & Julia Stone - Old Friend';StreamUrl='';
icyFormat :: Pattern Text
icyFormat = do
  choice
    [ has $ between (icyPrefix <> "='") ("';StreamUrl='" <> chars) chars
    , has $ between (icyPrefix <> "='") "';" chars] -- :(

-- run mplayer
player :: Line -> Shell Line
player url =
  let filtered = grep (prefix icyPrefix) (mplayer url)
  in sed icyFormat filtered

icon :: Line
icon =  unsafeTextToLine "media-playback-start"

command :: Command
command  = Cmd "radio" args radio ["radio.stations"]
  where args = [ "list", "play <station>"
               , "on", "off", "toggle"
               , "seek", "kees"
               , "np", "nps"]
