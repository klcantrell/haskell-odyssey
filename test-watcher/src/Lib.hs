{-# LANGUAGE OverloadedStrings #-} -- for FilePath literals

module Lib
    ( watch
    )
where

import           System.FSNotify
import           Control.Concurrent             ( threadDelay )
import           Control.Monad                  ( forever )


watch :: IO ()
watch = withManager $ \mgr -> do
    -- start a watching job (in the background)
    watchDir mgr          -- manager
             "."          -- directory to watch
             (const True) -- predicate
             print        -- action

    -- sleep forever (until interrupted)
    forever $ threadDelay 1000000
