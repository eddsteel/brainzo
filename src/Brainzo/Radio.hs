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
radio (Just c) ("list":rest)         = (list c, rest)
radio (Just c) ("play":station:rest) = (playByKey c station, rest)
radio (Just c) ("seek":rest)         = (seek c, rest)
radio (Just c) ("kees":rest)         = (kees c, rest)
radio _        ("off":rest)          = (off, rest)
radio _        ("np":rest)           = (np, rest)
radio Nothing  all                   = (err "radio needs some stations.", all)
radio _        (op:rest)             = (err (T.append (T.append "radio doesn't understand " op) "."), rest)


-- run mplayer, piping to logfile
mplayer        :: Text -> Shell ()
mplayer url     = do
  t <- inproc "mplayer" [url] empty
  endless

off            :: Shell ()
off             = cat [inproc "killall" ["-q", "mplayer"] empty >> endless
                      , withNP (\_ -> npfile >>= rm ) empty ]

play           :: Text -> Text -> Shell ()
play key url    = cat [off
                  , npfile >>= \f -> output f (return key)
                  , mplayer url
                  , np]

playByKey      :: Config -> Text -> Shell ()
playByKey c s   = orNoop (play s <$> specified c)
  where
    specified = withStations (Map.lookup s)

playNext         :: Direction -> Config -> Text -> Shell ()
playNext d c key  = uncurry play (next c)
  where
    streams Fwd = withStations (Prelude.concat . repeat . Map.toList)
    streams Bwd = withStations (Prelude.concat . repeat . reverse . Map.toList)
    second = head . tail
    next c = second (dropWhile ((/= key) . fst) (streams d c))

playFirst      :: Config -> Shell ()
playFirst c     = uncurry play first
  where first = withStations (head . Map.toList) c

list           :: Config -> Shell ()
list            = withStations (echo . T.unlines . Map.keys)

np             :: Shell ()
np              = withNP echo (echo "off")

seek           :: Config -> Shell ()
seek c          = withNP (playNext Fwd c) (playFirst c)

kees           :: Config -> Shell ()
kees c          = withNP (playNext Bwd c) (playFirst c)

withNP         :: (Text -> Shell ()) -> Shell () -> Shell ()
withNP f g      = do
                    nf <- npfile
                    b <- testfile nf
                    if b then input nf >>= f
                      else g

withStations   :: (Stations -> a) -> Config -> a
withStations f  = f . parseStations

parseStations  :: Config -> Stations
parseStations s = Map.fromList $ foldr tupleOrDrop [] (T.words <$> T.lines s)
  where tupleOrDrop [a, b] agg = (a, b):agg
        tupleOrDrop _      agg = agg
