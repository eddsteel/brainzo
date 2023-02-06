{-# LANGUAGE OverloadedStrings #-}
module Brainzo.Commands.Blinds where

import Brainzo.Data
import Brainzo.File(brainzoFile)
import Brainzo.Util(bail)
import Control.Monad(forM_)
import Data.Either.Utils
import Data.Ini.Config
import Data.List.NonEmpty(NonEmpty((:|)))
import Data.Time
import Data.Char(digitToInt)
import Network.Curl
import qualified Data.Text as T
import Turtle

blinds :: WorkStep
blinds args = case args of
  ("up":|rest)   -> cmd Up (blindspec rest)
  ("down":|rest) -> cmd Dn (blindspec rest)
  ("stop":|rest) -> cmd Sp (blindspec rest)
  _              -> bail "blinds" args

data BlindsConfig = Cfg { host :: Text, port :: Int, controllerID :: Text, blindIDs :: Text }
data BlindSpec = AllBlinds | SpecificBlinds [Int] deriving Show
data BlindCmd = Up | Dn | Sp deriving Show
type BlindID = Text

blindspec :: [Text] -> BlindSpec
blindspec []      = AllBlinds
blindspec ["all"] = AllBlinds
blindspec xs = SpecificBlinds $ (digitToInt . head . T.unpack) <$> xs

apply :: BlindSpec -> [BlindID] -> [BlindID]
apply AllBlinds ids = ids
apply (SpecificBlinds ixs) ids = fmap (\i -> ids !! (i - 1)) ixs

hash :: IO Text
hash = T.pack . take 6 . reverse . show . (`div` 1000) <$> picos
  where picos = diffTimeToPicoseconds . utctDayTime <$> getCurrentTime

configParser :: IniParser BlindsConfig
configParser = section "BLINDS" $ do
   host       <- fieldOf "host" string
   port       <- fieldOf "port" number
   controller <- fieldOf "id" string
   blindIDs   <- fieldOf "blinds" string
   return $ Cfg (T.pack host) port (T.pack controller) (T.pack blindIDs)

cmd :: BlindCmd -> BlindSpec -> Shell Line
cmd cmd spec = do
  fp <- brainzoFile "brainzo.conf"
  ini <- liftIO . strict . input $ fp
  let config = forceEither $ parseIniFile ini configParser
  let ids = apply spec $ T.words (blindIDs config)
  -- Haven't figured out how to do multiple commands in one curl
  let curl = curlBlinds config cmd
  _ <- liftIO . withCurlDo . forM_ ids $ curl
  empty

curlBlinds :: BlindsConfig -> BlindCmd -> BlindID -> IO ()
curlBlinds cfg cmd blind = do
  hsh <- hash
  curlGet (url hsh) []
  where url h = T.unpack . T.concat $
          [ "http://"
          , host cfg, ":"
          , T.pack . show . port $ cfg
          , "/neo/v1/transmit?id="
          , controllerID cfg, "&command="
          , blind, "-"
          , T.toLower . T.pack . show $ cmd
          , "&hash=", h]

command :: Command
command = Cmd "blinds" [ "up <blinds>"
                       , "down <blinds>"
                       , "stop <blinds>"] blinds []
