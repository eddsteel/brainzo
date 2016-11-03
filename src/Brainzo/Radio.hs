{-# LANGUAGE OverloadedStrings #-}
module Brainzo.Radio(radio) where

import Prelude hiding (FilePath)
import Brainzo.Data
import Control.Monad((=<<))
import Data.Maybe(fromMaybe)
import Data.Map(Map)
import qualified Data.Map as Map
import Filesystem.Path.CurrentOS hiding (empty)
import Data.Text(Text)
import qualified Data.Text as T
import Turtle

type Config    = Text
type Stations  = Map Text Text
data Direction = Bwd | Fwd

expandHome     :: FilePath -> Shell FilePath
expandHome fpa  = do
                    h <- format fp <$> home
                    return $ (fromText . T.concat) [h, "/", format fp fpa]

npfile         :: Shell FilePath
npfile          = expandHome ".radio-np"

logfile        :: Shell FilePath
logfile         = expandHome ".radio-log"

orNoop         :: Maybe (Shell a) -> Shell a
orNoop          = fromMaybe empty

radio                               :: Maybe Config -> [Text] -> (Shell (), [Text])
radio (Just c) ("list":rest)         = (withStations list c, rest)
radio (Just c) ("play":station:rest) = (((playByKey station) . parseStations) c, rest)
radio (Just c) ("seek":rest)         = (withStations seek c, rest)
radio (Just c) ("kees":rest)         = (withStations kees c, rest)
radio _        ("off":rest)          = (off, rest)
radio _        ("np":rest)           = (np, rest)
radio Nothing  all                   = (err "radio needs some stations.", all)
radio _        (op:rest)             = (err (T.append (T.append "radio doesn't understand " op) "."), rest)
radio _        []                    = (err usage, [])

withStations   :: (Stations -> a) -> Config -> a
withStations f  = f . parseStations

usage :: Text
usage = "radio list|play <station>|seek|kees|off|np"

mplayerOptions :: [Text]
mplayerOptions = ["-prefer-ipv4", "-ao", "alsa"]

-- ICY Info: StreamTitle='Angus & Julia Stone - Old Friend';StreamUrl='';
icyFormat :: Pattern Text
icyFormat = do
--  (title:_) <- chars `sepBy` "';"
  _ <- "ICY Info: StreamTitle="
  name <- between (char '\'') (char '\'') (star (notChar '\''))
  _ <- chars
  return name

-- run mplayer
mplayer        :: Text -> Shell ()
mplayer url     =
  let
    mplayerOut = inproc "mplayer" (url:mplayerOptions) empty
    filtered = grep ( prefix "ICY Info:") mplayerOut
    formatted = sed icyFormat filtered
  in
    stdout formatted

off            :: Shell ()
off             = cat [inproc "killall" ["-q", "mplayer"] empty >> endless
                      , withNP (\_ -> npfile >>= rm ) empty ]

play           :: Text -> Text -> Shell ()
play key url    = cat [off
                  , npfile >>= \f -> output f (return key)
                  , mplayer url
                  , np]

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

list           :: Stations -> Shell ()
list            = echo . T.unlines . Map.keys

np             :: Shell ()
np              = withNP echo (echo "off")

seek           :: Stations -> Shell ()
seek c          = withNP (playNext Fwd c) (playFirst c)

kees           :: Stations -> Shell ()
kees c          = withNP (playNext Bwd c) (playFirst c)

withNP         :: (Text -> Shell ()) -> Shell () -> Shell ()
withNP f g      = do
                    nf <- npfile
                    b <- testfile nf
                    if b then input nf >>= f
                      else g


-- TODO this seems to be effed now.
parseStations  :: Config -> Stations
parseStations s = Map.fromList $ foldr tupleOrDrop [] (T.words <$> T.lines s)
  where tupleOrDrop [a, b] agg = (a, b):agg
        tupleOrDrop _      agg = agg
