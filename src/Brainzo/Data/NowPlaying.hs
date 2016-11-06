module Brainzo.Data.NowPlaying where

import Data.Text
import Data.Time.Clock(UTCTime)

data NowPlaying = NP { npId :: Maybe Integer, station :: Text, track :: Text, on :: Maybe UTCTime } deriving Show
np i s t o = NP { npId = i, station = s, track = t, on = o }
