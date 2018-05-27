module SourceFetch.Status
  ( doStatus
  ) where

import           Common           (goToClonesDir)
import           Control.Monad    (mapM_)
import           Data.Semigroup   ((<>))
import           System.Directory (getCurrentDirectory, listDirectory)

doStatus :: IO ()
doStatus = do
  cd        <- getCurrentDirectory
  clonesDir <- goToClonesDir
  content   <- listDirectory clonesDir
  putStrLn $ "Current directory: " <> cd

  putStrLn "Managed repos: "
  mapM_ putStrLn content
