{-# LANGUAGE OverloadedStrings #-}
module Brainzo.GoogleMaps (googleMap, searchURL) where

import Data.Text(Text)
import qualified Data.Text as T
import Data.String.Utils (replace)
import Turtle

-- greedy unless first char is "
googleMap :: [Text] -> (Shell (), [Text])
googleMap  = greedyUnlessQuoted (browser . searchURL . T.unwords)

searchURL :: Text -> Text
searchURL  = (T.append "https://www.google.com/maps/search/") . (T.replace " " "+")

-- below here should move

-- if first argument is quoted, just use that. Otherwise slurp 'em all.
-- e.g. `map my cool query` will find a map for 'my cool query'
--      but `map "my cool query" play something.mp3" will find a map for 'my cool query' and leave 'play something.mp3' for further work
-- in practice this means running `map '"my cool query"' play something.mp3` from the shell so it's probably only useful from scripts.
-- however since greedy is normally what's wanted it seems OK for now.
--
greedyUnlessQuoted                          :: ([Text] -> Shell ()) -> [Text] -> (Shell (), [Text])
greedyUnlessQuoted f (q:rest)
   | T.head q == T.last q && T.head q == '"' = (f (T.words (T.init (T.tail q))), rest)
   | otherwise                               = (f (q:rest), [])

browser    :: Text -> Shell ()
browser url = do
                inproc "open" [url] empty -- this obviously sucks
                return ()
