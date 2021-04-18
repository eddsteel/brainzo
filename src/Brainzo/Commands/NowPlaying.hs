{-# LANGUAGE OverloadedStrings #-}
module Brainzo.Commands.NowPlaying(command, retrieveNP, storeNP) where

import Brainzo.Apps(consulSet, consulValue)
import Brainzo.Data
import Brainzo.Data.NowPlaying
import Brainzo.Util(bail)
import Brainzo.Processes(notifyPipe, encodeJSON, decodeJSON, decodeKV)
import Data.List.NonEmpty(NonEmpty((:|))) 
import Data.Time.Clock.POSIX(getPOSIXTime,POSIXTime)
import Turtle(Shell, Line, Text, liftIO, unsafeTextToLine, lineToText,echo)

nowPlaying :: WorkStep
nowPlaying args = case args of
  ("get":|_)        -> getNP
  ("display":|_)    -> displayNP
  ("set":|json:_)   -> parseJSAndSetNP json
  ("set-kv":|kvs:_) -> parseKVAndSetNP kvs
  _                 -> bail "np" args

getNP :: Shell Line
getNP = toLine <$> retrieveNP

retrieveNP :: Shell NowPlaying
retrieveNP = consulValue "now-playing" >>= decodeJSON

parseJSAndSetNP :: Text -> Shell Line
parseJSAndSetNP json = decodeJSON (unsafeTextToLine json) >>= storeNP

parseKVAndSetNP :: Text -> Shell Line
parseKVAndSetNP ks = decodeKV (unsafeTextToLine ks) >>= storeNP

storeNP  :: NowPlaying -> Shell Line
storeNP np = do
  json <- epoched np >>= encodeJSON  
  consulSet "now-playing" . lineToText $ json

displayNP :: Shell Line
displayNP = getNP >>= notifyPipe icon

epoched :: NowPlaying -> Shell NowPlaying
epoched np = stamped np <$> epoch

epoch :: Shell POSIXTime
epoch = liftIO getPOSIXTime

icon :: Line
icon = unsafeTextToLine "media-playback-start"

command :: Command
command  = Cmd "np" ["display", "get", "set <json>", "set-kv <k1=v1,k2=v2>"] nowPlaying []
