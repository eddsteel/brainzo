module Brainzo.Util where
import Data.Text

(\./) :: (String -> a) -> Text -> a
(\./) f = f . unpack

(/.\) :: (Text -> a) -> String -> a
(/.\) f = f . pack
