{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
module SourceFetch.Types where

import           Data.Text             (unpack)
import           SourceFetch.Init.Data
import           System.FilePath       ((</>))
import           System.Process.Typed  (ProcessConfig, setWorkingDir)

data RootLevel
data RepoLevel

data Env = Env
  { eCurrentDir :: FilePath
  }

data EnvWithRepo = EnvWithRepo
  { ewrEnv        :: Env
  , ewrRepoSource :: RepoSource
  }

data EnvWithProcess a = EnvWithProcess
  { ewpEnv     :: EnvWithRepo
  , ewpProcess :: EnvWithRepo -> ProcessConfig () () ()
  }

class SetProcess a where
  setCurrentDir :: a -> ProcessConfig () () ()

instance SetProcess (EnvWithProcess RootLevel) where
  setCurrentDir ewp@EnvWithProcess{..} = setWorkingDir (currentDir ewp) (ewpProcess ewpEnv)

instance SetProcess (EnvWithProcess RepoLevel) where
  setCurrentDir ewp@EnvWithProcess{..} = setWorkingDir (currentDir ewp </> rn) (ewpProcess ewpEnv)
   where
     rn = unpack $ unRepoName (rsName (repoSource ewp))

class HasCurrentDir a where
  currentDir :: a -> FilePath

instance HasCurrentDir Env where
  currentDir = eCurrentDir

instance HasCurrentDir EnvWithRepo where
  currentDir = eCurrentDir . ewrEnv

instance HasCurrentDir (EnvWithProcess a) where
  currentDir = eCurrentDir . ewrEnv . ewpEnv

class HasRepoSource a where
  repoSource :: a -> RepoSource

instance HasRepoSource EnvWithRepo where
  repoSource = ewrRepoSource

instance HasRepoSource (EnvWithProcess a) where
  repoSource = ewrRepoSource . ewpEnv
