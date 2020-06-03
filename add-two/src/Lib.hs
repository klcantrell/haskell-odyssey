module Lib
    ( addTwo
    )
where

import           System.Environment
import           Text.Read

addTwo :: IO ()
addTwo = do
    args <- getArgs
    handle args
  where
    handle []      = print "You need to pass a number in"
    handle (x : _) = case readMaybe x of
        Just x  -> print $ x + 2
        Nothing -> print "You need to pass a number in"
