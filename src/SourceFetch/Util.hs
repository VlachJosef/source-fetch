{-# LANGUAGE RecordWildCards #-}
module SourceFetch.Util
  ( execProcess
  , runAction
  ) where

import           Common                     (getAuth, goToClonesDir)
import           Control.Monad.Reader       (ReaderT, ask, runReaderT)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except (ExceptT (..))
import qualified Data.ByteString.Char8      as C
import qualified Data.ByteString.Lazy       as L
import           Data.Semigroup             ((<>))
import qualified GitHub.Data                as G (Repo, SimpleOwner (..), repoName, repoOwner, untagName)
import           SourceFetch                (readTest)
import           SourceFetch.Init.Data      (InitError, OrganisationName (..), RepoName (..), RepoSource (..))
import           SourceFetch.Types          (Env (..), HasCurrentDir (..), HasRepoSource (..), SetProcess (..))
import           System.Directory           (withCurrentDirectory)
import           System.Process.Typed       (readProcess)

runAction :: ([RepoSource] -> ReaderT Env IO ()) -> ExceptT InitError IO ()
runAction f = do
  _         <- lift getAuth
  --repos     <- hardcodedRepos
  repos     <- readTest
  --repos   <- githubRepos auth
  clonesDir <- lift goToClonesDir
  let repoNames = repoInfo repos
  lift $ withCurrentDirectory clonesDir (runReaderT (f repoNames) (Env clonesDir))
  where
     repoInfo :: [G.Repo] -> [RepoSource]
     repoInfo repos = (\repo -> RepoSource (OrganisationName . G.untagName . G.simpleOwnerLogin . G.repoOwner $ repo) (RepoName . G.untagName . G.repoName $ repo)) <$> repos

execProcess ::
  ( HasRepoSource a
  , HasCurrentDir a
  , SetProcess a
  ) => ReaderT a IO ()
execProcess = do
  ewp <- ask
  let processConfig = setCurrentDir ewp
  lift $ putStrLn $ show processConfig
  (exitCode, err, out) <- lift $ readProcess processConfig
  lift $ putStrLn $ "ExitCode: " <> show exitCode
  lift $ putStrLn $ "StdErr: " <> C.unpack (L.toStrict err)
  lift $ putStrLn $ "StdOut: " <> C.unpack (L.toStrict out)
