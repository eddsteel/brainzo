{-# LANGUAGE OverloadedStrings #-}
module Brainzo.Radio(radio, icyFormat) where

import Control.Arrow((>>>))
import Data.List(find)
import Data.Map(Map, toList)
import Data.Maybe(fromMaybe)
import Data.Text()
import Filesystem.Path.CurrentOS hiding (empty)
import Prelude hiding (FilePath)
import Turtle hiding (find)
import qualified Data.Map as Map
import qualified Data.Text as T

type Config    = Text
type Stations  = Map Text Text
data Direction = Bwd | Fwd

expandHome     :: FilePath -> Shell FilePath
expandHome fpa  = do
                    h <- format fp <$> home
                    return $ (fromText . T.concat) [h, "/", format fp fpa]

npfile         :: Shell FilePath
npfile          = expandHome ".radio-np"

orNoop         :: Maybe (Shell a) -> Shell a
orNoop          = fromMaybe empty

radio                               :: Maybe Config -> [Text] -> (Shell (), [Text])
radio (Just c) ("list":rest)         = doRadio c rest list
radio (Just c) ("play":station:rest) = doRadio c rest $ playByKey station
radio (Just c) ("seek":rest)         = doRadio c rest seek
radio (Just c) ("kees":rest)         = doRadio c rest kees
radio (Just c) ("on":rest)           = doRadio c rest on
radio _        ("off":rest)          = (off, rest)
radio _        ("np":rest)           = (np, rest)
radio Nothing  rest                  = (err "radio needs some stations.", rest)
radio _        (op:rest)             = (err (T.append (T.append "radio doesn't understand " op) "."), rest)
radio _        []                    = (err usage, [])

doRadio :: Config -> [Text] -> (Stations -> Shell()) -> (Shell (), [Text])
doRadio c rest f = ((parseStations >>> f) c, rest)

usage          :: Text
usage           = "radio list|play <station>|on|off|seek|kees|off|np"

list           :: Stations -> Shell ()
list            = echo . T.unlines . Map.keys

np             :: Shell ()
np              = withNP echo (echo "off")

seek           :: Stations -> Shell ()
seek c          = withNP (playNext Fwd c) (playFirst c)

kees           :: Stations -> Shell ()
kees c          = withNP (playNext Bwd c) (playFirst c)

on             :: Stations -> Shell ()
on c            = withNP (playCurrent c) (playFirst c)

off            :: Shell ()
off             = cat [inproc "killall" ["-q", "mplayer"] empty >> endless
                      , withNP (\_ -> npfile >>= rm ) empty ]

play           :: Text -> Text -> Shell ()
play key url    = cat [off
                  , npfile >>= \f -> output f (return key)
                  , mplayer url
                  , np]


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
mplayer        :: Text -> Shell ()
mplayer url     =
  let
    mplayerO  = inproc "mplayer" (url:mplayerOptions) empty
    filtered  = grep (prefix icyPrefix) mplayerO
    formatted = sed icyFormat filtered
  in
    stdout formatted

playByKey      :: Text -> Stations -> Shell ()
playByKey s ss  = orNoop (play s <$> Map.lookup s ss)

playNext         :: Direction -> Stations -> Text -> Shell ()
playNext d c key  = uncurry play (next c)
  where
    streams Fwd = Prelude.concat . repeat . Map.toList
    streams Bwd = Prelude.concat . repeat . reverse . Map.toList
    second = head . tail
    next c = second (dropWhile ((/= key) . fst) (streams d c))

playFirst      :: Stations -> Shell ()
playFirst       = (uncurry play . (head . Map.toList))

playCurrent    :: Stations -> Text -> Shell ()
playCurrent s k = uncurry play current
  where current = head $ filter ((== k) . fst) (toList s)

withNP         :: (Text -> Shell ()) -> Shell () -> Shell ()
withNP f g      = do
                    nf <- npfile
                    b <- testfile nf
                    if b
                      then input nf >>= f
                      else g

-- TODO this seems to be effed now.
parseStations  :: Config -> Stations
parseStations s = Map.fromList $ foldr tupleOrDrop [] (T.words <$> T.lines s)
  where tupleOrDrop [a, b] agg = (a, b):agg
        tupleOrDrop _      agg = agg
