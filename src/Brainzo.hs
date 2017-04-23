{-# LANGUAGE OverloadedStrings #-}
module Brainzo
       ( module Brainzo
       , module Brainzo.Data
       ) where

import Brainzo.Commands
import Brainzo.Data
import Control.Exception(Exception,catch,SomeException,bracket)
import Data.List.NonEmpty(NonEmpty((:|)))
import Data.Text(Text)
import Prelude hiding (FilePath, concat)
import System.Process(runCommand, waitForProcess)
import qualified Brainzo.DB.BrainzoDB as DB
import qualified Data.List.Utils as List
import qualified Data.Map as M
import qualified Data.Text as T
import Turtle

birth :: Shell Brainzo
birth = do
  e  <- loadEnv
  db <- liftIO DB.new
  return $ Brainzo e db

getToWork         :: Brainzo -> [Text] -> Shell Text
getToWork b (a:as) = goDo (M.lookup a commands) b as
getToWork _ []     = return ""

goDo :: Maybe Command -> Brainzo -> [Text] -> Shell Text
goDo Nothing _ _       = err brainzoUsage >> return ""
goDo (Just c) _ []     = err (usage c) >> return ""
goDo (Just c) b (t:ts) = chompOp (t:|ts) (entryPoint c)
  where
    chompOp (a:|as) step =
      let (workDone, rest) = step b (a:|as)
      in cat [workDone, getToWork b rest]

brainzoUsage :: Text
brainzoUsage = T.unlines . mconcat $
  [[ "I am Brainzo. Bleep bloop."
   , ""
   , "Known commands:"]
  , fmap ((T.append "  - ") . commandName) . M.elems $ commands
  , [ ""
    , "Share and Enjoy."]]

loadRequirement :: Text -> Shell Requirement
loadRequirement name = do
  h <- format fp <$> home
  let reqFile = T.concat [h, "/.brainzo/", name]
  contents <- (liftIO . readTextFile . fromText) reqFile
  return (name, contents)

loadEnv :: Shell Env
loadEnv  =
  do
    rs <- sequence . fmap loadRequirement . mconcat . fmap reqs . M.elems $ commands
    return . M.fromList $ rs
