module Brainzo.Data where
import Data.Text(Text)
import Data.Map(Map)

type Env = Map Text Text
type Requirement = (Text, Text) -- Requirement for a module: (<name>, <filename>)
