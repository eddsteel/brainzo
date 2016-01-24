module Brainzo.Operation where

data Operation = Compound [Operation]
               | RunCmd [String]
               | Say String
               | Err String
               | Write FilePath String
               | Read FilePath (String -> Operation) Operation
               | NoOp

cat           :: [Operation] -> Operation
cat ops        = Compound ops

run           :: [String] -> Operation
run s          = RunCmd s

say           :: String -> Operation
say s          = Say s

err           :: String -> Operation
err e          = Err e

writeO        :: String -> String -> Operation
writeO p s     = Write p s

readO         :: String -> (String -> Operation) -> Operation -> Operation
readO p f o    = Read p f o

noop          :: Operation
noop           = NoOp
