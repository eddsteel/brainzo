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
import Prelude hiding (FilePath, concat, unlines)
import Database.SQLite.Simple
import System.Directory(doesFileExist)

data RadioDB = DB Text

newDB :: IO RadioDB
newDB  = do
  path <- expandHomeIO ".brainzo/radio.db"
  let db = (DB path)
  -- we'll assume if the file exists there's a DB in it. No worries bruh.
  exists <- doesFileExist $ unpack path
  if exists
    then return db
    else initDB db >> return db

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
  execute_ c $ Query (
    unlines [ "create table radio_now_playing "
            , "(id integer primary key, station text, track text, playedOn timestamp);"])

instance NPStorage RadioDB where
  retrieve = withDB $ \c -> do
    np <- (query_ c $ Query (
              unlines [ "select id, station, track, playedOn "
                      , "from radio_now_playing "
                      , "order by id desc limit 1"])) :: IO [NowPlaying]
    return $ head np
  store np = withDB $ \c ->
    let st = station np
        tr = track np
    in do
      execute c (Query (
        unlines [ "insert into radio_now_playing (station, track, playedOn) "
                , "values (?, ?, datetime('now'));"])) (st, tr)
      return $ concat [st, " - ", tr]

main :: IO ()
main = do
  db <- newDB
  _ <- store (np Nothing Nothing "Hi" "Yo") db
  _ <- retrieve db
  return ()
