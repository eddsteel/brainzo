module Brainzo.Radio(radio) where

import Brainzo.Data
import Brainzo.Operation
import Control.Monad((=<<))
import Data.Maybe(fromMaybe)
import Data.Map(Map)
import qualified Data.Map as Map

type Config   = String
type Stations = Map String String

npfile         :: String
npfile          = "~/.radio-np"

orNoop         :: Maybe Operation -> Operation
orNoop          = fromMaybe noop

radio                               :: Maybe Config -> [String] -> (Operation, [String])
radio (Just c) ("list":rest)         = (list c, rest)
radio (Just c) ("play":station:rest) = (playByKey c station, rest)
radio (Just c) ("seek":rest)         = (seek c, rest)
radio _        ("off":rest)          = (off, rest)
radio _        ("np":rest)           = (np, rest)
radio Nothing  all                   = (err "radio needs some stations.", all)
radio _        (op:rest)             = (err ("radio doesn't understand " ++ op ++ "."), rest)

off            :: Operation
off             = cat [run ["killall", "mplayer"], run ["rm", "-f", npfile]]

play           :: String -> Operation
play url        = cat [ off
                      , writeO npfile url
                      , run ["mplayer", "-really-quiet", url]]

playByKey      :: Config -> String -> Operation
playByKey c s   = orNoop (play <$> specified)
  where
    specified = Map.lookup s (parseStations c)

playNext       :: Config -> String -> Operation
playNext c stn  = play next
  where
    stns = parseStations c
    streams = concat . repeat . (fmap snd) . Map.toList $ stns
    next = head (tail (dropWhile (/= stn) streams))

playFirst      :: Config -> Operation
playFirst c     = play first
  where first = (snd . head . Map.toList) $  parseStations c

list           :: Config -> Operation
list c          = say $ (unlines . Map.keys) $ parseStations c

np             :: Operation
np              = readO npfile say (say "off")

seek           :: Config -> Operation
seek c          = readO npfile (playNext c) (playFirst c)


parseStations  :: Config -> Stations
parseStations s = Map.fromList $ foldr tupleOrDrop [] (words <$> lines s)
  where tupleOrDrop [a, b] agg = (a, b):agg
        tupleOrDrop _      agg = agg
