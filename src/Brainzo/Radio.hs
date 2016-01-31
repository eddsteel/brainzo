module Brainzo.Radio(radio) where

import Brainzo.Data
import Brainzo.Operation
import Control.Monad((=<<))
import Data.Maybe(fromMaybe)
import Data.Map(Map)
import qualified Data.Map as Map

type Config    = String
type Stations  = Map String String
data Direction = Bwd | Fwd

npfile         :: String
npfile          = "~/.radio-np"

orNoop         :: Maybe Operation -> Operation
orNoop          = fromMaybe noop

radio                               :: Maybe Config -> [String] -> (Operation, [String])
radio (Just c) ("list":rest)         = (list c, rest)
radio (Just c) ("play":station:rest) = (playByKey c station, rest)
radio (Just c) ("seek":rest)         = (seek c, rest)
radio (Just c) ("kees":rest)         = (kees c, rest)
radio _        ("off":rest)          = (off, rest)
radio _        ("np":rest)           = (np, rest)
radio Nothing  all                   = (err "radio needs some stations.", all)
radio _        (op:rest)             = (err ("radio doesn't understand " ++ op ++ "."), rest)

off            :: Operation
off             = cat [run ["killall", "mplayer"], run ["rm", "-f", npfile]]

play           :: String -> String -> Operation
play key url    = cat [ off
                      , writeO npfile key
                      , run ["mplayer", "-really-quiet", url]]

playByKey      :: Config -> String -> Operation
playByKey c s   = orNoop (play s <$> specified c)
  where
    specified = withStations (Map.lookup s)

playNext       :: Direction -> Config -> String -> Operation
playNext d c key  = (uncurry play) (next c)
  where
    streams Fwd = concat . repeat . Map.toList . parseStations
    streams Bwd = concat . repeat . reverse . Map.toList . parseStations
    second = head . tail
    next c = second (dropWhile ((/= key) . fst) (streams d c))

playFirst      :: Config -> Operation
playFirst c     = (uncurry play) first
  where first = withStations (head . Map.toList) c

list           :: Config -> Operation
list            = withStations (\stns -> say $ (unlines . Map.keys) stns)

np             :: Operation
np              = readO npfile say (say "off")

seek           :: Config -> Operation
seek c          = readO npfile (playNext Fwd c) (playFirst c)

kees           :: Config -> Operation
kees c          = readO npfile (playNext Bwd c) (playFirst c)

withStations   :: (Stations -> a) -> Config -> a
withStations f  = f . parseStations

parseStations  :: Config -> Stations
parseStations s = Map.fromList $ foldr tupleOrDrop [] (words <$> lines s)
  where tupleOrDrop [a, b] agg = (a, b):agg
        tupleOrDrop _      agg = agg
