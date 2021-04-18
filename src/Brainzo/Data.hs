{-# LANGUAGE OverloadedStrings #-}

module Brainzo.Data where

import Brainzo.DB.BrainzoDB(DB)
import Data.List.NonEmpty(NonEmpty)
import Data.Map.Strict(Map)
import Turtle

data Brainzo = Brainzo { environment :: Env, database :: DB }
type Env = Map Text Text
type Requirement = (Text, Text) -- Requirement for a module: (<file name>, <file contents>)
type WorkStep = NonEmpty Text -> Shell Line
data Command = Cmd { commandName :: Text
                   , subCommands :: [Text]
                   , entryPoint :: WorkStep
                   , reqs :: [Text] -- files to load
                   }
cmdPair :: Command -> (Text, Command)
cmdPair c = (commandName c, c)
          
