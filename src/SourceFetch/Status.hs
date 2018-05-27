{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module SourceFetch.Status
  ( doStatus
  ) where

import           Common                    (goToClonesDir)
import           Data.Semigroup            ((<>))
import           System.Directory          (doesDirectoryExist, getCurrentDirectory, listDirectory)
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
repoInfo repo = info <$> exists "_git" <*> exists ".git"
  where
    exists :: String -> IO Bool
    exists dir = doesDirectoryExist ("clones" </> repo </> dir)

    info :: Bool -> Bool -> Info
    info underscoreGit dotGit = Info repo $ case (underscoreGit, dotGit) of
     (True, _) -> Locked
     (_, True) -> Open
     _         -> Unknown
