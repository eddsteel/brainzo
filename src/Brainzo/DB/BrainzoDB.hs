module Brainzo.DB.BrainzoDB(DB, new, radioDB) where

import Brainzo.DB.RadioDB(RadioDB)
import qualified Brainzo.DB.RadioDB as Radio

data DB = DB (RadioDB) --, other dbs

radioDB :: DB -> RadioDB
radioDB (DB r) = r

new :: IO DB
new = do
  radio <- Radio.newDB
  return $ DB (radio)
