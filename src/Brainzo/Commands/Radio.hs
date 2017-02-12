{-# LANGUAGE OverloadedStrings #-}
module Brainzo.Commands.Radio(command) where

import Brainzo.Apps(mplayer, notify)
import Brainzo.DB.BrainzoDB(radioDB)
import Brainzo.DB.RadioDB
import Brainzo.Data
import Brainzo.Data.NowPlaying(NowPlaying, fromStationTrack, toStationTrack, station)
import Brainzo.File(brainzoFile)
import qualified Brainzo.Data.Storage as DB
import Data.List.NonEmpty(NonEmpty((:|)))
import Data.Map.Strict(Map, toList)
import Data.Maybe(fromMaybe, isJust, fromJust)
import Prelude hiding (FilePath)
import Turtle((<>), FilePath, Pattern, Shell, Text)
import Turtle(between, cat, chars, choice, echo, empty, endless, err, grep, has)
import Turtle(inproc, input, liftIO, output, prefix, rm, sed, stdout, testfile)
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
  | noConfig  = (err "radio needs some stations.", a:as)
  | otherwise = case args of
      ("list":|r)   -> ((list . parseStations) b, r)
      ("play":|k:r) -> doRadio b r (playByKey k)
      ("seek":|r)   -> doRadio b r seek
      ("kees":|r)   -> doRadio b r kees
      ("on":|r)     -> doRadio b r on
      ("np":|r)     -> doRadio b r np
      ("nps":|r)    -> doRadio b r nps
      ("off":|r)    -> (off, r)
      (op:|_)       -> bail $ T.concat ["radio doesn't understand ", op, "."]
  where noConfig = isJust . Map.lookup "radio" . environment $ b
        bail t = (err t, a:as)

doRadio :: Brainzo -> [Text] -> (Stations -> RadioDB -> Shell ()) -> (Shell (), [Text])
doRadio b rest fn =
  let
    db = database b
    config = parseStations b
    action = fn config . radioDB $ db
  in
    (action, rest)

list :: Stations -> Shell ()
list = echo . T.unlines . Map.keys

np :: Stations -> RadioDB -> Shell ()
np _ db         = withNP nowPlaying (echo "off")
  where nowPlaying :: Text -> Shell ()
        nowPlaying _ = liftIO $ fmap toStationTrack (DB.retrieve db) >>= echo

nps :: Stations -> RadioDB -> Shell ()
nps _ db = liftIO $ station <$> DB.retrieve db >>= echo

seek :: Stations -> RadioDB -> Shell ()
seek c db = withNP (playNext Fwd c db) (playFirst c db)

kees :: Stations -> RadioDB -> Shell ()
kees c db = withNP (playNext Bwd c db) (playFirst c db)

on :: Stations -> RadioDB -> Shell ()
on c db = withNP (playCurrent c db) (playFirst c db)

off :: Shell ()
off = cat [inproc "killall" ["-q", "mplayer"] empty >> endless
                      , withNP (\_ -> npfile >>= rm ) empty ]

play :: RadioDB -> Text -> Text -> Shell ()
play db key url = cat [off
                      , npfile >>= \file -> output file (return key)
                      -- play radio, piping NP to store. Start with empty string
                      -- so that we get an entry for stations that don't publish
                      -- now playing info.
                      , player url >>= store . fromStationTrack key >>= notify >>= echo]
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

playByKey :: Text -> Stations -> RadioDB -> Shell ()
playByKey s ss db = orNoop (play db s <$> Map.lookup s ss)

playNext :: Direction -> Stations -> RadioDB -> Text -> Shell ()
playNext d c db key  = uncurry (play db) next
  where
    streams Fwd = Prelude.concat . repeat . Map.toList
    streams Bwd = Prelude.concat . repeat . reverse . Map.toList
    second = head . tail
    next = second (dropWhile ((/= key) . fst) (streams d c))

playFirst :: Stations -> RadioDB -> Shell ()
playFirst s db  = (uncurry (play db) . (head . Map.toList)) s

playCurrent :: Stations -> RadioDB -> Text -> Shell ()
playCurrent s db k = uncurry (play db) current
  where current = head $ filter ((== k) . fst) (toList s)

withNP :: (Text -> Shell ()) -> Shell () -> Shell ()
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

command :: Command
command  = Cmd "radio" args radio ["radio.stations"]
  where args = [ "list", "play <station>"
               , "on", "off"
               , "seek", "kees"
               , "np", "nps"]
