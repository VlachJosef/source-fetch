{-# LANGUAGE GADTs            #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}

module SourceFetch.Init
  ( execInit
  , lockDotGit
  , unlockDotGit
  ) where

import           Common                     (ifDirExistsT_, toExceptT, toExceptTM)
import           Control.Monad.Reader       (ReaderT, ask, withReaderT)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import           Control.Newtype.Generics   (op)
import           Data.Functor               (void)
import qualified Data.Map.Strict            as Map (toList)
import           Data.Semigroup             ((<>))
import           Data.Text                  (unpack)
import qualified Data.Vector                as V (Vector, fromList)
import qualified GitHub                     (Auth)
import qualified GitHub.Data                as Github (Repo, RepoPublicity (..))
import qualified GitHub.Endpoints.Repos     as Github (mkOrganizationName, organizationRepos')
import           SourceFetch.FakeRepos      (genesis, mkRepo)
import           SourceFetch.Init.Data      (InitError (..), OrganisationName (..), RepoName (..), RepoSource (..))
import           SourceFetch.Types          (Env (..), EnvWithProcess (..), EnvWithRepo (..), HasCurrentDir (..),
                                             HasRepoSource (..), RootLevel)
import           SourceFetch.Util           (execProcess, runAction)
import           System.Directory           (doesDirectoryExist, renameDirectory)
import           System.FilePath            ((</>))
import           System.IO                  (writeFile)
import           System.Process.Typed       (ProcessConfig, proc, readProcessStdout)

gitInit :: ReaderT Env IO ()
gitInit = do
  Env{..} <- ask
  let dotGit = eCurrentDir </> ".git"
  ifDirExistsT_ dotGit
    (putStrLn $ "Directory " <> dotGit <> " already exists.")
    (readProcessStdout (proc "git" ["init"]))

createGlobalDotIgnore :: ReaderT Env IO ()
createGlobalDotIgnore = do
  Env{..} <- ask
  lift $ writeFile (eCurrentDir </> ".gitignore") "_git"

doesRepoExists :: ReaderT EnvWithRepo IO Bool
doesRepoExists = do
  EnvWithRepo{..} <- ask
  let repoDir = currentDir ewrEnv </> unpack (unRepoName (rsName ewrRepoSource))
  lift $ doesDirectoryExist repoDir

renameDotGit ::
  ( HasRepoSource a
  , HasCurrentDir a
  )
  => FilePath
  -> FilePath
  -> ReaderT a IO ()
renameDotGit from to = do
  a <- ask
  let cd            = currentDir a
      rep           = unpack (op RepoName (rsName (repoSource a)))
      repoDir       = cd </> rep
      dotGit        = repoDir </> from
      dotGitRenamed = repoDir </> to
  ifDirExistsT_ dotGit
    (renameDirectory dotGit dotGitRenamed)
    (putStrLn $ "Directory " <> dotGit <> " doesn't exist.")

lockDotGit ::
  ( HasRepoSource a
  , HasCurrentDir a
  ) => ReaderT a IO ()
lockDotGit = renameDotGit ".git" "_git"

unlockDotGit ::
  ( HasRepoSource a
  , HasCurrentDir a
  ) => ReaderT a IO ()
unlockDotGit = renameDotGit "_git" ".git"

runInit :: [RepoSource] -> ReaderT Env IO ()
runInit xs = do
  gitInit
  createGlobalDotIgnore
  void $ traverse processRepo xs

processRepo :: RepoSource -> ReaderT Env IO ()
processRepo rs = withReaderT (flip EnvWithRepo rs) $ do
  exists <- doesRepoExists
  if exists
    then do
      EnvWithRepo{..} <- ask
      let repoDir = currentDir ewrEnv </> unpack (unRepoName (rsName ewrRepoSource))
      lift $ putStrLn $ "Repo " <> repoDir <> " already exists."
    else
      atRootLevel gitClone *>
      lockDotGit *>
      atRootLevel (const gitAdd) *>
      atRootLevel gitCommit

atRootLevel :: (EnvWithRepo -> ProcessConfig () () ()) -> ReaderT EnvWithRepo IO ()
atRootLevel f = withReaderT (flip (EnvWithProcess @RootLevel) f) execProcess

gitAdd :: ProcessConfig () () ()
gitAdd = proc "git" ["add", "."]

gitCommit, gitClone :: EnvWithRepo -> ProcessConfig () () ()
gitCommit EnvWithRepo{..} = proc "git" ["commit", "-m", "Initialize repository - " <> unpack (op RepoName $ rsName ewrRepoSource)]

gitClone EnvWithRepo{..} = proc "git" ["clone", "git@github.com:" <> unpack (op OrganisationName $ rsOrganisation ewrRepoSource) <> "/" <> unpack (op RepoName $ rsName ewrRepoSource) <> ".git"]

hardcodedRepos :: ExceptT InitError IO (V.Vector Github.Repo)
hardcodedRepos = toExceptT GError (Right repos)
  where
    repos :: V.Vector Github.Repo
    repos = V.fromList $ uncurry mkRepo <$> Map.toList genesis

githubRepos :: Maybe GitHub.Auth -> ExceptT InitError IO (V.Vector Github.Repo)
githubRepos auth = toExceptTM GError (Github.organizationRepos' auth (Github.mkOrganizationName "org-name") Github.RepoPublicityPublic)

execInit :: IO ()
execInit = do
  result <- runExceptT doInit
  case result of
    Left (GError githubError) -> putStrLn $ "GithubError " <> show githubError
    Left (PError parseError)  -> putStrLn $ "ParseError " <> show parseError
    Right a                   -> pure a

doInit :: ExceptT InitError IO ()
doInit = runAction runInit
