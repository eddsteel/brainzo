{-# LANGUAGE OverloadedStrings #-}
module Brainzo(dispatch, loadEnv, parseArgs) where

import Prelude hiding (FilePath, concat)
import Brainzo.Data
import Brainzo.GoogleMaps(googleMap)
import qualified Brainzo.Radio as Radio
import System.Process(runCommand, waitForProcess)
import Control.Exception(Exception,catch,SomeException,bracket)
import qualified Data.List.Utils as List
import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified System.IO.Strict as Strict
import Filesystem.Path.CurrentOS hiding (empty)
import Turtle

dispatch                :: Env -> [Text] -> Shell ()
dispatch e as            = cat (parseArgs e as)

loadRequirement :: Text -> Shell (Text, Text)
loadRequirement name = do
  h <- format fp <$> home
  let f = T.concat [h, "/.", name]
  contents <- (liftIO . readTextFile . fromText) f
  return (name, contents)

loadEnv :: Shell Env
loadEnv = do
  r <- loadRequirement "radio"
  -- load other requirements here.
  return (Map.fromList [r])

parseArgs               :: Env -> [Text] -> [Shell ()]
parseArgs e ("map":as)   = chompOp e googleMap as
parseArgs e ("radio":as) = chompOp e (Radio.radio (Map.lookup "radio" e)) as
parseArgs _ []           = []
parseArgs _ _            = [err "wat."]

chompOp e o as           = op : parseArgs e remnant
  where (op,remnant) = o as
