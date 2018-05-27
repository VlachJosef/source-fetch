{-# LANGUAGE RecordWildCards #-}

module SourceFetch.Init
  ( execInit
  ) where

import           Common                        (getAuth, goToClonesDir)
import           Control.Monad.Reader          (ReaderT, ask, runReaderT, withReaderT)
import           Control.Monad.Trans.Class     (MonadTrans, lift)
import           Control.Monad.Trans.Except    (ExceptT (..), runExceptT, withExceptT)
import           Data.Functor                  (void)
import qualified Data.Map.Strict               as Map (toList)
import           Data.Maybe                    (catMaybes)
import           Data.Semigroup                ((<>))
import qualified Data.Vector                   as Vector (Vector, fromList, toList)
import qualified GitHub                        (Auth)
import qualified GitHub.Data                   as Github (Error, Repo, RepoPublicity (..), repoSshUrl)
import qualified GitHub.Endpoints.Repos        as Github (mkOrganizationName, organizationRepos')
import           SourceFetch.FakeRepos         (genesis, mkRepo)
import           SourceFetch.Init.Data         (OrganisationName (..), RepoName (..))
import           SourceFetch.Init.SshUrlParser (parseSshUrl)
import           System.Directory              (doesDirectoryExist, renameDirectory, withCurrentDirectory)
import           System.FilePath               ((</>))
import           System.IO                     (writeFile)
import           System.Process.Typed          (ProcessConfig, proc, readProcessStdout)
import           Text.Parsec                   (ParseError)

data Env = Env
  { currentDir :: FilePath
  }

data EnvWithRepo = EnvWithRepo
  { env              :: Env
  , repoName         :: RepoName
  , organisationName :: OrganisationName
  }

fromEnv :: OrganisationName -> RepoName -> Env -> EnvWithRepo
fromEnv ornName repoName env = EnvWithRepo env repoName ornName

predicateM :: (MonadTrans t, Monad m, Monad (t m)) => (a -> m Bool) -> a -> m b -> m c -> t m ()
predicateM predicate value onTrue onFalse = do
  success <- lift $ predicate value
  lift $ if success
    then void onTrue
    else void onFalse

ifDirExists :: FilePath -> IO a -> IO b -> ReaderT c IO ()
ifDirExists = predicateM doesDirectoryExist

gitInit :: ReaderT Env IO ()
gitInit = do
  Env{..} <- ask
  let dotGit = currentDir </> ".git"
  ifDirExists dotGit
    (putStrLn $ "Directory " <> dotGit <> " already exists.")
    (readProcessStdout (proc "git" ["init"]))

createGlobalDotIgnore :: ReaderT Env IO ()
createGlobalDotIgnore = do
  Env{..} <- ask
  lift $ writeFile (currentDir </> ".gitignore") "_git"

cloneRepo :: ReaderT EnvWithRepo IO ()
cloneRepo = do
  EnvWithRepo{..} <- ask
  let repoDir = currentDir env </> unRepoName repoName
  ifDirExists repoDir
    (putStrLn $ "Repo " <> unRepoName repoName <> " already exists.")
    (readProcessStdout (gitClone organisationName repoName))

gitClone :: OrganisationName -> RepoName -> ProcessConfig () () ()
gitClone organisationName repoName = proc "git" ["clone", "git@github.com:" <> unOrganisationName organisationName <> "/" <> unRepoName repoName <> ".git"]

renameDotGit :: ReaderT EnvWithRepo IO ()
renameDotGit = do
  EnvWithRepo{..} <- ask
  let repoDir       = currentDir env </> unRepoName repoName
      dotGit        = repoDir </> ".git"
      dotGitRenamed = repoDir </> "_git"
  ifDirExists dotGit
    (renameDirectory dotGit dotGitRenamed)
    (putStrLn $ "Directory " <> dotGit <> " doesn't exist.")

commitRepo :: ReaderT EnvWithRepo IO ()
commitRepo = do
  EnvWithRepo{..} <- ask
  let gitAdd, gitCommit :: ProcessConfig () () ()
      gitAdd    = proc "git" ["add", "."]
      gitCommit = proc "git" ["commit", "-m", "Initialize repository - " <> unRepoName repoName]
  lift . void . readProcessStdout $ gitAdd
  lift . void . readProcessStdout $ gitCommit

runInit :: [(OrganisationName, RepoName)] -> ReaderT Env IO ()
runInit xs = do
  gitInit
  createGlobalDotIgnore
  void . sequence $ uncurry processRepo <$> xs

processRepo :: OrganisationName -> RepoName -> ReaderT Env IO ()
processRepo organisation repo = withReaderT (fromEnv organisation repo) $ cloneRepo *>
                                                                          renameDotGit *>
                                                                          commitRepo

data InitError
  = GError Github.Error
  | PError ParseError

toExceptT :: Monad m => (a -> b) -> Either a c -> ExceptT b m c
toExceptT f e = toExceptTM f (pure e)

toExceptTM :: Monad m => (a -> b) -> m (Either a c) -> ExceptT b m c
toExceptTM f e = withExceptT f $ ExceptT e

hardcodedRepos :: ExceptT InitError IO (Vector.Vector Github.Repo)
hardcodedRepos = toExceptT GError (Right repos)
  where
    repos :: Vector.Vector Github.Repo
    repos = Vector.fromList $ uncurry mkRepo <$> Map.toList genesis

githubRepos :: Maybe GitHub.Auth -> ExceptT InitError IO (Vector.Vector Github.Repo)
githubRepos auth = toExceptTM GError (Github.organizationRepos' auth (Github.mkOrganizationName "org-name") Github.RepoPublicityPublic)

execInit :: IO ()
execInit = do
  result <- runExceptT doInit
  case result of
    (Left (GError githubError)) -> putStrLn $ "GithubError " <> show githubError
    (Left (PError parseError))  -> putStrLn $ "ParseError " <> show parseError
    (Right a)                   -> pure a

doInit :: ExceptT InitError IO ()
doInit = do
  auth      <- lift getAuth
  repos     <- hardcodedRepos
  --repos   <- githubRepos auth
  clonesDir <- lift goToClonesDir
  repoNames <- toExceptT PError $ repoInfo repos
  lift $ withCurrentDirectory clonesDir (runReaderT (runInit repoNames) (Env clonesDir))
  where
     repoInfo :: Vector.Vector Github.Repo -> Either ParseError [(OrganisationName, RepoName)]
     repoInfo repos = sequence $ parseSshUrl <$> (catMaybes . Vector.toList $ (Github.repoSshUrl <$> repos))
