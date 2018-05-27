{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module SourceFetch.Init (
  execInit
  , doStatus
  ) where

import           Common                                   (getAuth)
import           Control.Monad.Extra                      (unlessM)
import           Control.Monad.Reader                     (ReaderT, ask, runReaderT, withReaderT)
import           Control.Monad.Trans.Class                (MonadTrans, lift)
import           Control.Monad.Trans.Except               (ExceptT (..), runExceptT, withExceptT)
import           Data.Functor                             (void)
import qualified Data.Map.Strict                          as Map (Map, fromList, toList)
import           Data.Maybe                               (catMaybes)
import           Data.Semigroup                           ((<>))
import qualified Data.Text                                as Text
import qualified Data.Vector                              as Vector
import qualified GitHub                                   (Auth)
import qualified GitHub.Data                              as Github
import qualified GitHub.Endpoints.Repos                   as Github (mkOrganizationName, organizationRepos')
import           System.Directory                         (createDirectory, doesDirectoryExist, getCurrentDirectory,
                                                           renameDirectory, withCurrentDirectory)
import           System.FilePath                          ((</>))
import           System.IO                                (writeFile)
import           System.Process.Typed                     (ProcessConfig, proc, readProcessStdout)
import           Text.Parsec                              (ParseError, parse)
import           Text.Parsec.Text                         (Parser)
import           Text.ParserCombinators.Parsec.Char       (satisfy, string)
import           Text.ParserCombinators.Parsec.Combinator (many1)

newtype RepoName = RepoName
  { unRepoName :: String
  }

newtype OrganisationName = OrganisationName
  { unOrganisationName :: String
  }

data Env = Env
  { currentDir :: FilePath
  }

data EnvWithRepo = EnvWithRepo
  { env              :: Env
  , repoName         :: RepoName
  , organisationName :: OrganisationName
  }

predicateM :: (MonadTrans t, Monad m, Monad (t m)) => (a -> m Bool) -> a -> m b -> m c -> t m ()
predicateM predicate value onTrue onFalse = do
  success <- lift $ (predicate value)
  lift $ if success
    then void onTrue
    else void onFalse

ifDirExists :: FilePath -> IO a -> IO b -> ReaderT c IO ()
ifDirExists = predicateM doesDirectoryExist

parseMe :: Github.URL -> Either ParseError (OrganisationName, RepoName)
parseMe parsee = parse pp "repo-parser" (Github.getUrl parsee)
  where

    pp :: Parser (OrganisationName, RepoName)
    pp = do
       org <- organisationParser
       name <- repoParser
       pure (org, name)

    repoParser :: Parser RepoName
    repoParser = string "/" *> (RepoName <$> many1 (satisfy ((/=) '.'))) <* string ".git"

    organisationParser :: Parser OrganisationName
    organisationParser = string "git@github.com:" *> (OrganisationName <$> many1 (satisfy ((/=) '/')))

gitInit :: ReaderT Env IO ()
gitInit = do
  Env{..} <- ask
  let dotGit = currentDir </> ".git"
  ifDirExists dotGit
    (putStrLn $ "Directory " <> dotGit <> " already exist.")
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
      gitAdd = proc "git" ["add", "."]
      gitCommit = proc "git" ["commit", "-m", ("Initialize repository - " <> unRepoName repoName)]
  lift . void . readProcessStdout $ gitAdd
  lift . void . readProcessStdout $ gitCommit

runInit :: [(OrganisationName, RepoName)] -> ReaderT Env IO ()
runInit xs = do
  gitInit
  createGlobalDotIgnore
  void . sequence $ uncurry processRepo <$> xs

processRepo :: OrganisationName -> RepoName -> ReaderT Env IO ()
processRepo organisation repo = withReaderT (\a -> EnvWithRepo a repo organisation) $ cloneRepo *>
                                                      renameDotGit *>
                                                      commitRepo

goToClonesDir :: IO FilePath
goToClonesDir = do
  clonesDir <- (</> "clones") <$> getCurrentDirectory

  clonesDir <$ unlessM
    (doesDirectoryExist clonesDir)
    (createDirectory clonesDir)

doStatus :: IO ()
doStatus = do
  cd <- getCurrentDirectory
  putStrLn $ "Current directory: " <> cd

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

githubRepos :: Maybe (GitHub.Auth) -> ExceptT InitError IO (Vector.Vector Github.Repo)
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
  auth      <- lift $ getAuth
  repos     <- hardcodedRepos
  --repos   <- githubRepos auth
  clonesDir <- lift $ goToClonesDir
  repoNames <- toExceptT PError $ repoInfo repos
  lift $ withCurrentDirectory clonesDir (runReaderT (runInit repoNames) (Env clonesDir))
  where
     repoInfo :: Vector.Vector Github.Repo -> Either ParseError [(OrganisationName, RepoName)]
     repoInfo repos = sequence $ parseMe <$> (catMaybes . Vector.toList $ (Github.repoSshUrl <$> repos))

genesis :: Map.Map Text.Text Text.Text
genesis = Map.fromList
  [ ("fpco", "monad-unlift")
  , ("Gabriel439", "Haskell-MMorph-Library")
  , ("mvv", "transformers-base")
  , ("basvandijk", "monad-control")
  ]

owner :: Text.Text -> Github.SimpleOwner
owner organisation = let simpleOwnerId        = Github.mkOwnerId 1
                         simpleOwnerLogin     = Github.mkOwnerName organisation
                         simpleOwnerUrl       = Github.URL $ "https://api.github.com/users/" <> organisation
                         simpleOwnerAvatarUrl = Github.URL "https://avatars1.githubusercontent.com/u/5656336?v=4"
                         simpleOwnerType      = Github.OwnerOrganization
                     in Github.SimpleOwner {..}

makeRepoSshUrl, makeRepoHtmlUrl, makeRepoUrl, makeRepoHooksUrl :: Text.Text -> Text.Text -> Github.URL
makeRepoSshUrl   organisation repoName = Github.URL $ "git@github.com:" <> organisation <> "/" <> repoName <> ".git"
makeRepoHtmlUrl  organisation repoName = Github.URL $ "https://github.com/" <> organisation <> "/" <> repoName
makeRepoUrl      organisation repoName = Github.URL $ "https://api.github.com/repos/" <> organisation <> "/" <> repoName
makeRepoHooksUrl organisation repoName = Github.URL (Github.getUrl (makeRepoUrl organisation repoName) <> "/hooks")

mkRepo :: Text.Text -> Text.Text -> Github.Repo
mkRepo org rName =
    let
      repoSshUrl = Just (makeRepoSshUrl org rName)
      repoDescription = Nothing
      repoCreatedAt   = Nothing
      repoHtmlUrl     = makeRepoHtmlUrl org rName
      repoSvnUrl      = Nothing
      repoForks       = Nothing
      repoHomepage    = Nothing
      repoFork        = Nothing
      repoGitUrl      = Nothing
      repoPrivate     = False
      repoArchived    = False
      repoCloneUrl    = Nothing
      repoSize        = Nothing
      repoUpdatedAt   = Nothing
      repoWatchers    = Nothing
      repoOwner       = owner org
      repoName        = Github.mkRepoName rName
      repoLanguage    = Nothing
      repoDefaultBranch   = Nothing
      repoPushedAt        = Nothing
      repoId              = Github.mkRepoId 1
      repoUrl             = makeRepoUrl org rName
      repoOpenIssues      = Nothing
      repoHasWiki         = Nothing
      repoHasIssues       = Nothing
      repoHasDownloads    = Nothing
      repoParent          = Nothing
      repoSource          = Nothing
      repoHooksUrl        = makeRepoHooksUrl org rName
      repoStargazersCount = 0
    in Github.Repo {..}
