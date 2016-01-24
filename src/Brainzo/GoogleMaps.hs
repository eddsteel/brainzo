module Brainzo.GoogleMaps (googleMap, searchURL) where

import Data.String.Utils (replace)
import Brainzo.Operation

-- greedy unless first char is "
googleMap :: [String] -> (Operation, [String])
googleMap  = greedyUnlessQuoted (browser . searchURL . unwords)

searchURL :: String -> String
searchURL  = ("https://www.google.com/maps/search/" ++) . (replace " " "+")

-- below here should move

-- if first argument is quoted, just use that. Otherwise slurp 'em all.
-- e.g. `map my cool query` will find a map for 'my cool query'
--      but `map "my cool query" play something.mp3" will find a map for 'my cool query' and leave 'play something.mp3' for further work
-- in practice this means running `map '"my cool query"' play something.mp3` from the shell so it's probably only useful from scripts.
-- however since greedy is normally what's wanted it seems OK for now.
--
greedyUnlessQuoted                    :: ([String] -> Operation) -> [String] -> (Operation, [String])
greedyUnlessQuoted f (q:rest)
   | head q == last q && head q == '"' = (f (words (init (tail q))), rest)
   | otherwise                         = (f (q:rest), [])

browser    :: String -> Operation
browser url = run ["open", url] -- this obviously sucks
