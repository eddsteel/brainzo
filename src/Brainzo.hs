module Brainzo(dispatch, loadEnv) where

import Brainzo.Data
import Brainzo.Operation(Operation (..))
import Brainzo.GoogleMaps(googleMap)
import qualified Brainzo.Radio as Radio
import System.Process(runCommand, waitForProcess)
import Control.Exception(Exception,catch,SomeException,bracket)
import Data.List.Utils(join)
import qualified Data.Map as Map
import qualified System.IO.Strict as Strict
import System.IO(stderr, hPutStrLn,hGetContents,openFile,IOMode(..),hClose)
import System.Directory(getHomeDirectory)

replaceHead  :: (Eq a) => a -> [a] -> [a] -> [a]
replaceHead a as (b:bs)
  | a == b    = as ++ bs
  | otherwise = (b:bs)

loadRequirement :: String -> IO (String, String)
loadRequirement name = do
  h <- getHomeDirectory
  contents <- readFile $ concat [h,"/.",name]
  return (name, contents)

loadEnv :: IO Env
loadEnv = do
  r <- loadRequirement "radio"
  return (Map.fromList [r])

dispatch              :: Env -> [String] -> IO ()
dispatch e args        = executeAll (parseArgs e args)

executeAll            :: [Operation] -> IO ()
executeAll             = runAll . fmap execute

runAll                :: [IO ()] -> IO ()
runAll []              = return ()
runAll (i:is)          = i >> runAll is

handleO :: Operation -> SomeException -> IO ()
handleO o _ = execute o

execute               :: Operation -> IO ()
execute (Compound ops) = executeAll ops
execute (Say s)        = putStrLn s
execute (Err s)        = hPutStrLn stderr s
execute (NoOp)         = return ()
execute (Write fp s)   = do
  h <- getHomeDirectory
  let fp' = replaceHead '~' h fp
  _ <- writeFile fp' s
  return ()
execute (Read fp f o)    = do
  h <- getHomeDirectory
  let fp' = replaceHead '~' h fp
  (Strict.readFile fp' >>= execute . f) `catch` (handleO o) -- in case next op uses file.
execute (RunCmd s)     = do
  putStrLn $ "executing '" ++ cmd ++ "'"
  pid <- runCommand cmd
  _ <- waitForProcess pid -- TODO: use exit code -- make exit an operation
  return ()
  where cmd = join " " s

chompOp       :: Env -> ([String] -> (Operation,[String])) -> [String] -> [Operation]
chompOp e o as = op : parseArgs e remnant
  where (op,remnant) = o as

parseArgs               :: Env -> [String] -> [Operation]
parseArgs e ("map":as)   = chompOp e googleMap as
parseArgs e ("radio":as) = chompOp e (Radio.radio (Map.lookup "radio" e)) as
parseArgs _ []           = [NoOp]
parseArgs _ _            = [Err "wat."]

-- todo string to assistance matching based on shortest unambiguous prefix
