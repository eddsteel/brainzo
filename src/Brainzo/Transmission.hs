{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Brainzo.Transmission(transmission) where

import Control.Applicative
import Control.Monad.Except
import Data.Char(isSpace)
import Data.ConfigFile(ConfigParser, get, CPError, readstring, emptyCP)
import Data.Text(Text)
import Data.Fraction
import qualified Data.Text as T
import Turtle

data TransmissionConfig = TC { host :: Text, seedRatio :: Double }
-- TODO would like progress to be Fraction. Is that a bad idea?
data TransmissionStatus = TS { id :: Int, progress :: Double, name :: Text }
type MagnetURI = Text
type Config = Text

transmission                                :: Maybe Config -> [Text] -> (Shell (), [Text])
transmission (Just cp) ("add":(magnet:as)) = (withConfig cp (add magnet), as)
transmission (Just cp) ("list":rest)       = (withConfig cp list, rest)
transmission Nothing all                   = (err "Need some config in ~/.transmission", all)

fakeproc :: Text -> [Text] -> Shell Text -> Shell ()
fakeproc cmd args stdout = do
  proc "echo" (cmd:args) stdout
  return ()

list              :: TransmissionConfig -> Shell ()
list TC {host = h} = do
  proc "transmission-remote" [h, "-l"] empty
  return ()

add                                   :: MagnetURI -> TransmissionConfig -> Shell ()
add mu cp@TC{host = h, seedRatio = sr} = do
  proc "transmission-remote" [h, "-a", mu] empty
  list cp

withConfig     :: Config -> (TransmissionConfig -> Shell ())  -> Shell ()
withConfig cp f = (either bail f) (useConfig cp)
  where bail = err . T.pack . show


readtext     :: MonadError CPError m => ConfigParser -> Text -> m ConfigParser
readtext cp t = readstring cp (T.unpack t)

useConfig  :: Config -> Either CPError TransmissionConfig
useConfig c =
  do
    cp <- readtext emptyCP c
    TC <$>
      (T.pack <$> get cp "DEFAULT" "host") <*>
      (read <$> get cp "DEFAULT" "seed-ratio")


-- | Parse some text into a transmission status
--
-- >>>  parseStatus "   6   100%    1.36 GB  Done         0.0     0.0    3.0  Finished     LinuxDistribution.iso"
-- TS {id=6,progress=1.0,name=LinuxDistribution.iso}
--
-- TODO real parsers would be less shit
--
parseStatus :: Text -> Maybe TransmissionStatus
parseStatus t = let
  s1 = T.stripStart t
  (i, s2) = T.breakOn " " s1
  s3 = T.stripStart s2
  (p, s4) = T.breakOn " " s3
  n = T.takeWhileEnd (not . isSpace) s4
  in do
    id <- (read . T.unpack) i
    prog <- parseProgress p
    return (TS id prog n)



parseProgress :: Text -> Maybe Double
parseProgress t
  | last (T.unpack t) == '%' = let
      s = T.unpack t
      n = init s
      m = read n :: Double
      in
       Just (m / 100)
  | otherwise = Nothing
