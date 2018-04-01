{-# LANGUAGE OverloadedStrings #-}
module Brainzo.Commands.Radio(command,icyFormat,list,seek,kees,play,on,off,np,nps) where

import Brainzo.Apps(mplayer)
import Brainzo.DB.BrainzoDB(radioDB)
import Brainzo.DB.RadioDB
import Brainzo.Data
import Brainzo.Data.NowPlaying(NowPlaying, fromStationTrack, toLine, station)
import Brainzo.File(brainzoFile)
import qualified Brainzo.Data.Storage as DB
import Brainzo.Notify
import Control.Applicative((<|>))
import Data.List.NonEmpty(NonEmpty((:|)))
import qualified Data.List.NonEmpty as NEL
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import Data.Maybe(fromMaybe, isJust, fromJust)
import Prelude hiding (FilePath)
import Turtle((<>), FilePath, Pattern, Shell, Text)
import Turtle(between, chars, choice, empty, err, grep, has, Line, lineToText, textToLines)
import Turtle(inproc, input, liftIO, output, prefix, rm, sed, testfile, unsafeTextToLine)
import qualified Data.Text as T

type Stations  = Map Line Line
data Direction = Bwd | Fwd

npfile :: Shell FilePath
npfile = brainzoFile "radio-np"

pidfile :: Shell FilePath
pidfile = brainzoFile "radio.pid"

orNoop :: Maybe (Shell a) -> Shell a
orNoop = fromMaybe empty

radio :: WorkStep
radio b args @ (a:|as)
  | noConfig  = (err "radio needs some stations." >> mempty , a:as)
  | otherwise = case args of
      ("list":|r)   -> (list b, r)
      ("play":|k:r) -> (play (unsafeTextToLine k) b, r)
      ("seek":|r)   -> (seek b, r)
      ("kees":|r)   -> (kees b, r)
      ("on":|r)     -> (on b, r)
      ("off":|r)    -> (off, r)
      ("np":|r)     -> (np b, r)
      ("nps":|r)    -> (nps b, r)
      (op:|_)       -> bail . unsafeTextToLine . T.concat $ ["radio doesn't understand ", op, "."]
  where noConfig = isJust . Map.lookup "radio" . environment $ b
        bail t = (err t >> mempty, a:as)

list :: Brainzo -> Shell Lines
list b  = return stations
  where
    stations = Map.keys (parseStations b)

seek :: Brainzo -> Shell [Line]
seek  = withStationsAndDB $ \ss db -> withNP (playNext Fwd ss db) (playFirst ss db)

kees :: Brainzo -> Shell [Line]
kees  = withStationsAndDB $ \ss db -> withNP (playNext Bwd ss db) (playFirst ss db)

play  :: Line -> Brainzo -> Shell [Line]
play k = withStationsAndDB $ \ss db -> orNoop (playNamed db k <$> Map.lookup k ss)

on :: Brainzo -> Shell [Line]
on  = withStationsAndDB $ \ss db -> withNP (playCurrent ss db) (playFirst ss db)

off :: Shell [Line]
off =  (:[]) <$> (inproc "killall" ["-q", "mplayer"] empty)
       <|> (withNP (\_ -> npfile >>= rm >> mempty) mempty)

np :: Brainzo -> Shell [Line]
np b = retrieve b >>= notify
  where retrieve :: Brainzo -> Shell [Line]
        retrieve = withStationsAndDB $ \_ db -> withNP (nowPlaying db) (return ["off"])
        nowPlaying :: RadioDB -> Line -> Shell [Line]
        nowPlaying db _ =  liftIO $ fmap ((:[]) . toLine) (DB.retrieve db)
        notify :: Lines -> Shell Lines
        notify = notifyPipe icon . head

nps :: Brainzo -> Shell [Line]
nps  = withStationsAndDB $ \_ db -> liftIO $ fmap (NEL.toList . textToLines . station) (DB.retrieve db)

-- helpers
withStationsAndDB :: (Stations -> RadioDB -> Shell a) -> Brainzo -> Shell a
withStationsAndDB fn b =
  let
    db = radioDB $ database b
    config = parseStations b
  in
   fn config db


playNamed :: RadioDB -> Line -> Line -> Shell [Line]
playNamed db key url = off
  <|> (npfile >>= \file -> output file (return key) >> mempty)
  -- play radio, piping NP to store. Start with empty string
  -- so that we get an entry for stations that don't publish
  -- now playing info.
  <|> player url >>= store . name >>= notifyAllPipe icon
  where store  :: NowPlaying -> Shell [Text]
        store n = liftIO ((:[]) <$> DB.store n db)
        name :: Lines -> NowPlaying
        name ls = fromStationTrack (lineToText key) (lineToText (head ls))


icyPrefix :: Pattern Text
icyPrefix = "ICY Info: StreamTitle"

-- ICY Info: StreamTitle='Angus & Julia Stone - Old Friend';StreamUrl='';
icyFormat :: Pattern Text
icyFormat = do
  choice
    [ has $ between (icyPrefix <> "='") ("';StreamUrl='" <> chars) chars
    , has $ between (icyPrefix <> "='") "';" chars] -- :(

-- run mplayer
player :: Line -> Shell [Line]
player url =
  let filtered = grep (prefix icyPrefix) (mplayer url)
  in (:[]) <$> sed icyFormat filtered

playNext :: Direction -> Stations -> RadioDB -> Line -> Shell [Line]
playNext d c db key  = uncurry (playNamed db) next
  where
    streams Fwd = Prelude.concat . repeat . Map.toList
    streams Bwd = Prelude.concat . repeat . reverse . Map.toList
    second = head . tail
    next = second (dropWhile ((/= key) . fst) (streams d c))

playFirst :: Stations -> RadioDB -> Shell [Line]
playFirst s db  = (uncurry (playNamed db) . (head . Map.toList)) s

playCurrent :: Stations -> RadioDB -> Line -> Shell [Line]
playCurrent s db k = uncurry (playNamed db) current
  where current = head $ filter ((== k) . fst) (Map.toList s)

withNP :: (Line -> Shell [Line]) -> Shell [Line] -> Shell [Line]
withNP f g =
  do
    nf <- npfile
    b <- testfile nf
    if b
      then input nf >>= f
      else g

parseStations :: Brainzo -> Stations
parseStations (Brainzo env _) =
  Map.fromList $ foldr tupleOrDrop [] (T.words <$> T.lines config)
  where tupleOrDrop [a, b] agg = (line a, line b):agg
        tupleOrDrop _      agg = agg
        line = unsafeTextToLine
        config = fromJust . Map.lookup "radio.stations" $ env

icon :: Line
icon =  unsafeTextToLine "media-playback-start"

command :: Command
command  = Cmd "radio" args radio ["radio.stations"]
  where args = [ "list", "play <station>"
               , "on", "off"
               , "seek", "kees"
               , "np", "nps"]
