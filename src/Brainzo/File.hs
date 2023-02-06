{-# LANGUAGE OverloadedStrings #-}
module Brainzo.File where

import Data.Map(Map)
import qualified Data.Map as Map
import qualified Control.Foldl as Fold
import qualified Data.Text as T
import Filesystem.Path.CurrentOS hiding (empty)
import Prelude hiding (FilePath)
import Turtle

toList :: Shell a -> IO [a]
toList s = fold s Fold.list

expandHome    :: FilePath -> Shell FilePath
expandHome fpa = do
  h <- format fp <$> home
  return $ (fromText . T.concat) [h, "/", format fp fpa]

expandHomeIO  :: FilePath -> IO Text
expandHomeIO f = do
  fs <- (toList . expandHome) f
  return (format fp (head fs))

brainzoFile :: Text -> Shell FilePath
brainzoFile t = expandHome . fromText . T.concat $ [".config/brainzo/", t]

configMap :: FilePath -> Shell (Map Text Text)
configMap fp = do
  lines <- T.lines . lineToText <$> input fp
  let list = T.breakOn " " <$> lines
  return . Map.map T.strip . Map.fromList $ list
