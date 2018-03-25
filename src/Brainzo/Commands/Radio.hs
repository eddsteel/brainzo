{-# LANGUAGE OverloadedStrings #-}
module Brainzo.Commands.Radio(command,icyFormat,list,seek,kees,play,on,off,np,nps) where

import Brainzo.Apps(mplayer)
import Brainzo.DB.BrainzoDB(radioDB)
import Brainzo.DB.RadioDB
import Brainzo.Data
import Brainzo.Data.NowPlaying(NowPlaying, fromStationTrack, toStationTrack, station)
import Brainzo.File(brainzoFile)
import qualified Brainzo.Data.Storage as DB
import Brainzo.Notify(notifyPipe)
import Control.Applicative((<|>))
import Data.List.NonEmpty(NonEmpty((:|)))
import Data.Map.Strict(Map, toList)
import Data.Maybe(fromMaybe, isJust, fromJust)
import Prelude hiding (FilePath)
import Turtle((<>), FilePath, Pattern, Shell, Text)
import Turtle(between, chars, choice, empty, err, grep, has)
import Turtle(inproc, input, liftIO, output, prefix, rm, sed, testfile)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

type Stations  = Map Text Text
data Direction = Bwd | Fwd

npfile :: Shell FilePath
npfile = brainzoFile "radio-np"

pidfile :: Shell FilePath
pidfile = brainzoFile "radio.pid"

orNoop :: Maybe (Shell a) -> Shell a
orNoop = fromMaybe empty

radio :: WorkStep
radio b args @ (a:|as)
  | noConfig  = (err "radio needs some stations." >> return "", a:as)
  | otherwise = case args of
      ("list":|r)   -> (list b, r)
      ("play":|k:r) -> (play k b, r)
      ("seek":|r)   -> (seek b, r)
      ("kees":|r)   -> (kees b, r)
      ("on":|r)     -> (on b, r)
      ("off":|r)    -> (off, r)
      ("np":|r)     -> (np b, r)
      ("nps":|r)    -> (nps b, r)
      (op:|_)       -> bail $ T.concat ["radio doesn't understand ", op, "."]
  where noConfig = isJust . Map.lookup "radio" . environment $ b
        bail t = (err t >> return "", a:as)


list :: Brainzo -> Shell Text
list  = return . T.unlines . Map.keys . parseStations

seek :: Brainzo -> Shell Text
seek  = withStationsAndDB $ \ss db -> withNP (playNext Fwd ss db) (playFirst ss db)

kees :: Brainzo -> Shell Text
kees  = withStationsAndDB $ \ss db -> withNP (playNext Bwd ss db) (playFirst ss db)

play  :: Text -> Brainzo -> Shell Text
play k = withStationsAndDB $ \ss db -> orNoop (playNamed db k <$> Map.lookup k ss)

on :: Brainzo -> Shell Text
on  = withStationsAndDB $ \ss db -> withNP (playCurrent ss db) (playFirst ss db)

off :: Shell Text
off = (inproc "killall" ["-q", "mplayer"] empty)
      <|> (withNP (\_ -> npfile >>= rm >> return "" ) empty)

np :: Brainzo -> Shell Text
np  = withStationsAndDB $ \_ db -> withNP (nowPlaying db) (return "off") >>= ntfy
  where nowPlaying :: RadioDB -> Text -> Shell Text
        nowPlaying db _ = (liftIO $ fmap toStationTrack (DB.retrieve db))
        ntfy = notifyPipe icon

nps :: Brainzo -> Shell Text
nps  = withStationsAndDB $ \_ db -> liftIO $ station <$> DB.retrieve db

-- helpers

withStationsAndDB :: (Stations -> RadioDB -> Shell Text) -> Brainzo -> Shell Text
withStationsAndDB fn b =
  let
    db = radioDB $ database b
    config = parseStations b
  in
   fn config db

playNamed :: RadioDB -> Text -> Text -> Shell Text
playNamed db key url = off
  <|> (npfile >>= \file -> output file (return key) >> return "")
  -- play radio, piping NP to store. Start with empty string
  -- so that we get an entry for stations that don't publish
  -- now playing info.
  <|> (player url >>= store . fromStationTrack key >>= notifyPipe icon)
  where store  :: NowPlaying -> Shell Text
        store n = liftIO (DB.store n db)

icyPrefix :: Pattern Text
icyPrefix = "ICY Info: StreamTitle"

-- ICY Info: StreamTitle='Angus & Julia Stone - Old Friend';StreamUrl='';
icyFormat :: Pattern Text
icyFormat = do
  choice
    [ has $ between (icyPrefix <> "='") ("';StreamUrl='" <> chars) chars
    , has $ between (icyPrefix <> "='") "';" chars] -- :(

-- run mplayer
player :: Text -> Shell Text
player url =
  let filtered = grep (prefix icyPrefix) (mplayer url)
  in sed icyFormat filtered

playByKey :: Text -> Stations -> RadioDB -> Shell Text
playByKey s ss db = orNoop (playNamed db s <$> Map.lookup s ss)

playNext :: Direction -> Stations -> RadioDB -> Text -> Shell Text
playNext d c db key  = uncurry (playNamed db) next
  where
    streams Fwd = Prelude.concat . repeat . Map.toList
    streams Bwd = Prelude.concat . repeat . reverse . Map.toList
    second = head . tail
    next = second (dropWhile ((/= key) . fst) (streams d c))

playFirst :: Stations -> RadioDB -> Shell Text
playFirst s db  = (uncurry (playNamed db) . (head . Map.toList)) s

playCurrent :: Stations -> RadioDB -> Text -> Shell Text
playCurrent s db k = uncurry (playNamed db) current
  where current = head $ filter ((== k) . fst) (toList s)

withNP :: (Text -> Shell Text) -> Shell Text -> Shell Text
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
  where tupleOrDrop [a, b] agg = (a, b):agg
        tupleOrDrop _      agg = agg
        config = fromJust . Map.lookup "radio.stations" $ env

icon :: Text
icon =  "media-playback-start"

command :: Command
command  = Cmd "radio" args radio ["radio.stations"]
  where args = [ "list", "play <station>"
               , "on", "off"
               , "seek", "kees"
               , "np", "nps"]
