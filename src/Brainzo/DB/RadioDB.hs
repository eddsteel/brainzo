{-# LANGUAGE OverloadedStrings #-}
module Brainzo.DB.RadioDB
       ( NowPlaying
       , RadioDB
       , newDB
       , main
       , dbFile) where

import Brainzo.File(expandHomeIO)
import Brainzo.Data.NowPlaying
import Brainzo.Data.Storage
import Data.Text hiding (head)
import Prelude hiding (FilePath, concat)
import Filesystem.Path.CurrentOS hiding (empty, concat)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Turtle(sh)

data RadioDB = DB Text deriving Show

instance FromRow NowPlaying where
  fromRow = np <$> field <*> field <*> field <*> field

instance FromRow Int where
  fromRow = field

newDB :: IO RadioDB
newDB  = do
  fp <- DB <$> expandHomeIO ".brainzo/radio.db"
  -- if DB doesn't exist, run init DB
  -- TODO ^ how to work out "exists"?
  return fp

dbFile :: RadioDB -> String
dbFile (DB f) = unpack f

withDB :: (Connection -> IO a) -> RadioDB -> IO a
withDB f r = do
  conn <- open (dbFile r)
  val <- f conn
  close conn
  return val

initDB :: RadioDB -> IO ()
initDB  = withDB $ \c -> do
  execute_ c "create table radio_now_playing (id integer primary key, station text, track text, playedOn date);"

instance NPStorage RadioDB where
  retrieve = withDB $ \c -> do
    np <- (query_ c "select * from radio_now_playing order by id desc limit 1") :: IO [NowPlaying]
    mapM print np
    return $ head np
  store np = withDB $ \c ->
    let st = station np
        tr = track np
    in do
      execute c "insert into radio_now_playing (station, track, playedOn) values (?, ?, datetime('now'));"
        (st, tr)
      return $ concat [st, " - ", tr]

main :: IO ()
main = do
  db <- newDB
  store (np Nothing "Hi" "Yo" Nothing) db
  _ <- retrieve db
  return ()
