{-# LANGUAGE OverloadedStrings #-}
module Brainzo.Data.Mouse(readDirection, deltaT, MouseDirection) where

import Data.Text(Text)
import qualified Data.Text as T

data MouseDirection = XY Int Int

readDirection :: Text -> Maybe MouseDirection
readDirection "N" = Just $ XY 0 (-1)
readDirection "S" = Just $ XY 0 1
readDirection "E" = Just $ XY 1 0
readDirection "W" = Just $ XY (-1) 0
readDirection "NW" = Just $ XY (-1) (-1)
readDirection "NE" = Just $ XY 1 (-1)
readDirection "SW" = Just $ XY (-1) 1
readDirection "SE" = Just $ XY 1 1
readDirection _ = Nothing

deltaT :: MouseDirection -> Int -> [Text]
deltaT d n = fmap (T.pack . show) $ delta d n

delta :: MouseDirection -> Int -> [Int]
delta (XY x y) n = [x * n, y * n]
