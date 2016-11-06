module Brainzo.Data.Storage( NPStorage
                           , retrieve
                           , store
                           )  where

import Brainzo.Data.NowPlaying

class NPStorage s where
  retrieve :: s -> IO NowPlaying
  store    :: NowPlaying -> s -> IO ()
