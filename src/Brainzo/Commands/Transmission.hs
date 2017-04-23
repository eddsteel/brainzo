{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Brainzo.Commands.Transmission(transmission) where

import Control.Applicative
import Control.Monad.Except
import Data.Char(isSpace,isDigit)
import Data.ConfigFile(ConfigParser, get, CPError, readstring, emptyCP)
import Data.Text(Text)
import qualified Data.Text as T
import Data.Fraction(Fraction)
import qualified Data.Fraction as F
import Text.Regex.Applicative.Text
import Turtle hiding (spaces, chars, match) -- TODO replace regex parsing with Turtle's built in stuff

data TransmissionConfig = TC { host :: Text, seedRatio :: Double } deriving (Show, Eq)
data TransmissionStatus = TS { idNum :: Int, progress :: Fraction, name :: Text } deriving (Show,Eq)

instance Show Fraction where
  show = show . F.toPercentage

instance Eq Fraction where
  f == g = (F.toFactor f) == (F.toFactor g)

type MagnetURI = Text
type Config = Text

transmission                            :: Maybe Config -> [Text] -> (Shell (), [Text])
transmission (Just cp) ("add":magnet:as) = (withConfig cp (add magnet), as)
transmission (Just cp) ("list":rest)     = (withConfig cp list, rest)
transmission Nothing all                 = (err "Need some config in ~/.brainzo./transmission", all)

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
-- >>>  parseStatus (T.pack "   6   100%    1.36 GB  Done         0.0     0.0    3.0  Finished     Linux Distribution.iso")
-- Just (TS {idNum = 6, progress = 100.0, name = "Linux Distribution.iso"})`
--
parseStatus :: Text -> Maybe TransmissionStatus
parseStatus  = match reStatus

reStatus :: RE' TransmissionStatus
reStatus = TS <$> int <*>
                  reProgress <*>
                  (chars <* chars <* chars <* doubS <* doubS <* doubS <* chars *> reName)

spaces :: RE' String
spaces = many (psym isSpace)

chars :: RE' String
chars = spaces *> many (psym (not . isSpace))

int :: RE' Int
int = read <$> intS

intS :: RE' String
intS = spaces *> many (psym isDigit)

doubS :: RE' String
doubS = (\a b c -> a ++ (b : c)) <$> intS <*> sym '.' <*> many (psym isDigit)

num :: RE' Double
num = read <$> (doubS <|> intS)

reProgress :: RE' Fraction
reProgress = F.fromNumber (0.0, 100.0) <$> num <* sym '%'

reName :: RE' Text
reName = T.pack <$> (spaces *> many anySym)
