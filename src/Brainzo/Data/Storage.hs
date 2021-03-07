module Brainzo.Data.Storage( NPStorage
                           , retrieve
                           , store
                           )  where

import Brainzo.Data.NowPlaying
import Data.Text

class NPStorage s where
  retrieve :: s -> IO NowPlaying
  store    :: NowPlaying -> s -> IO Text

