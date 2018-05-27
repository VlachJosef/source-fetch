{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module SourceFetch.Status
  ( doStatus
  ) where

import           Common                    (goToClonesDir, ifDirExists)
import           Data.Semigroup            ((<>))
import           System.Directory          (getCurrentDirectory, listDirectory)
import           System.FilePath           ((</>))

import           Data.Data                 (Data)
import qualified GHC.Generics              as G (Generic)
import qualified Text.PrettyPrint.Tabulate as T (CellValueFormatter, Tabulate, ppTable)

data RepoMode
  = Locked
  | Open
  | Unknown
  deriving (Data, G.Generic, Show)

data Info = Info { path :: FilePath
                 , mode :: RepoMode
                 } deriving (Data, G.Generic)

instance T.CellValueFormatter RepoMode
instance T.Tabulate RepoMode
instance T.Tabulate Info

doStatus :: IO ()
doStatus = do
  cd        <- getCurrentDirectory
  clonesDir <- goToClonesDir
  repos     <- listDirectory clonesDir
  putStrLn $ "Current directory: " <> cd

  putStrLn "Managed repos: "
  modes <- sequence $ repoInfo <$> repos

  T.ppTable modes

repoInfo :: FilePath -> IO Info
repoInfo repo =
  (Info repo) <$> ifDirExists (repoDir </> "_git")
    (pure Locked)
    (ifDirExists (repoDir </> ".git")
      (pure Open)
      (pure Unknown))
  where
    repoDir = "clones" </> repo
