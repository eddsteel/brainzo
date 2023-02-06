{-# LANGUAGE OverloadedStrings #-}
module Brainzo
       ( module Brainzo
       , module Brainzo.Data
       ) where

import Brainzo.Commands
import Brainzo.Data
import Data.Maybe(fromJust)
import Data.List.NonEmpty(NonEmpty((:|)))
import Data.Text(Text)
import Prelude hiding (FilePath, concat)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as M
import qualified Data.Text as T
import Turtle

getToWork         :: [Text] -> Shell Line
getToWork ("bleep":_) = return . fromJust . textToLine $ "bloop!"
getToWork (a:as)      = goDo (M.lookup a commands) as
getToWork []          = mempty

goDo :: Maybe Command -> [Text] -> Shell Line
goDo Nothing _        = traverse err brainzoUsage >> mempty
goDo (Just c) []      = err (usage c) >> mempty
goDo (Just c) (a:rgs) = entryPoint c $ a:|rgs

brainzoUsage :: NonEmpty Line
brainzoUsage = texts >>= textToLines
  where
    texts = NEL.fromList . mconcat $
      [[ "I am Brainzo. Bleep bloop."
       , ""
       , "Known commands:"]
      , fmap ((T.append "  - ") . commandName) . M.elems $ commands
      , [ ""
        , "Share and Enjoy."]]
