module Brainzo.Data where

import Data.Map(Map)

type Env = Map String String
type Requirement = (String, String) -- Requirement for a module: (<name>, <filename>)
