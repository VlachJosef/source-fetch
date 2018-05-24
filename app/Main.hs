{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import           Common
import           Control.Monad                            (forM, forM_, unless, (<=<))
import           Control.Monad.Reader                     (ReaderT, ask, runReaderT)
import           Control.Monad.Trans.Class                (lift)
import           Data.ByteString.Lazy                     as DBL hiding (putStrLn, writeFile)
import qualified Data.ByteString.Lazy                     as L
import           Data.Functor                             (void, ($>), (<$))
import           Data.List                                (intercalate)
import qualified Data.Map.Strict                          as Map (Map, fromList,
                                                                  toList)
import           Data.Maybe                               (catMaybes)
import           Data.Maybe                               (fromMaybe)
import           Data.Semigroup                           ((<>))
import qualified Data.Text                                as Text
import qualified Data.Text.Lazy                           as TextL
import           Data.Text.Lazy.Encoding                  as Enc
import           Data.Time.Calendar                       (toGregorian)
import           Data.Time.Clock                          (UTCTime (..), getCurrentTime)
import           Data.Time.LocalTime                      (TimeOfDay (..), localDay, localTimeOfDay, utc,
                                                           utcToLocalTime)
import qualified Data.Vector                              as Vector
import           GHC.IO.Exception                         (ExitCode)
import qualified GitHub.Data                              as Github
import qualified GitHub.Endpoints.Repos                   as Github
import qualified GitHub.Endpoints.Search                  as Github
import           System.Directory                         (createDirectory, doesDirectoryExist, getCurrentDirectory,
                                                           renameDirectory, setCurrentDirectory)
import           System.Environment                       (getArgs)
import           System.FilePath                          ((</>))
import           System.IO                                (writeFile)
import           System.Process.Typed
import           Text.Parsec                              (ParseError, parse, parserFail, space, spaces, try)
import           Text.Parsec.Text                         (Parser)
import           Text.ParserCombinators.Parsec.Char       (alphaNum, anyChar, digit, satisfy, string)
import           Text.ParserCombinators.Parsec.Combinator (anyToken, between, eof, many1)


ensureDirExists :: FilePath -> IO ()
ensureDirExists dir = do
  dirExists <- doesDirectoryExist dir
  unless dirExists (createDirectory dir)

cloneIfNotExists :: String -> String -> IO FilePath
cloneIfNotExists organisationName repoName = do
  cd <- getCurrentDirectory
  let repoDir = cd </> repoName
  dirExists <- doesDirectoryExist repoDir
  if dirExists
    then putStrLn ("Repo " <> repoName <> " already exists.") $> repoDir
    else readProcessStdout (git organisationName repoName) $> repoDir

parseMe :: Github.URL -> Either ParseError (String, String)
parseMe parsee = parse pp "repo-parser" (Github.getUrl parsee)
  where

    pp :: Parser (String, String)
    pp = do
       org <- organisationParser
       name <- repoParser
       pure (org, name)

    repoParser :: Parser String
    repoParser = string "/" *> many1 (satisfy ((/=) '.')) <* string ".git"

    organisationParser :: Parser String
    organisationParser = string "git@github.com:" *> many1 (satisfy ((/=) '/'))

cloneRepo :: (String, String) -> ReaderT Env IO FilePath
cloneRepo repoInfo = lift $ (uncurry cloneIfNotExists) repoInfo

renameDotGit :: FilePath -> ReaderT Env IO ()
renameDotGit = lift . renameDotGit_
  where
    renameDotGit_ :: FilePath -> IO ()
    renameDotGit_ filePath = do
      dirExists <- doesDirectoryExist filePath
      if dirExists
        then putStrLn ("Directory .git in" <> filePath <> " doesn't exist.")
        else renameDirectory (filePath </> ".git") (filePath </> "_git")

createGlobalDotIgnore :: ReaderT Env IO ()
createGlobalDotIgnore = do
  Env{..} <- ask
  lift $ writeFile (currentDir </> ".gitignore") "_git"

data Env = Env
  { currentDir :: FilePath
  }

commitAll :: ReaderT Env IO ()
commitAll = do
  Env{..} <- ask
  dirExists <- lift $ doesDirectoryExist (currentDir </> ".git")
  if dirExists
    then lift $ putStrLn ("Directory .git in" <> currentDir <> " already exist.")
    else do
      readProcessStdout gitInit
      readProcessStdout gitAdd
      void . readProcessStdout $ gitCommit
  where
    gitInit, gitAdd, gitCommit :: ProcessConfig () () ()
    gitInit = proc "git" ["init"]
    gitAdd = proc "git" ["add", "."]
    gitCommit = proc "git" ["commit", "-m", "Initialize repository"]


wwww :: [(String, String)] -> ReaderT Env IO ()
wwww xs = do
  void . sequence $ (renameDotGit <=< cloneRepo) <$> xs
  createGlobalDotIgnore
  commitAll

main = do
  auth <- getAuth
  --result <- Github.organizationRepos' auth (Github.mkOrganizationName "org-name") Github.RepoPublicityPublic
  result <- pure $ (Right repos2 :: Either Github.Error (Vector.Vector Github.Repo))

  cd <- getCurrentDirectory

  let cloneDirs = cd </> "clones"

  ensureDirExists cloneDirs

  setCurrentDirectory cloneDirs

  putStrLn cloneDirs

  let
    sss :: Either ParseError [(String, String)]
    sss = sequence $ parseMe <$> (catMaybes . Vector.toList $ (Github.repoSshUrl <$> repos2))

  case sss of
    Left errors     -> putStrLn $ "Parse errors" <> show errors
    Right repoNames -> do
      cd <- getCurrentDirectory
      runReaderT (wwww repoNames) (Env cd)

  case result of
    Left e  -> putStrLn $ "Error: " ++ show e
    Right repos -> do

      let

      forM_ repos (\a -> putStrLn $
                         "repoSshUrl   : " ++ show (Github.repoSshUrl a) ++ "\n" ++
                         "repoHtmlUrl  : " ++ show (Github.repoHtmlUrl a) ++ "\n" ++
                         "repoName     : " ++ show (Github.repoName a) ++ "\n" ++
                         "repoId       : " ++ show (Github.repoId a) ++ "\n" ++
                         "repoUrl      : " ++ show (Github.repoUrl a) ++ "\n" ++
                         "repoHooksUrl : " ++ show (Github.repoHooksUrl a) ++ "\n" ++
                         "repoOwner    : " ++ show (Github.repoOwner a) ++ "\n"
                       )

      --putStrLn $ Text.unpack $ b

owner :: Text.Text -> Github.SimpleOwner
owner organisation = let simpleOwnerId        = Github.mkOwnerId 1
                         simpleOwnerLogin     = Github.mkOwnerName organisation
                         simpleOwnerUrl       = Github.URL $ "https://api.github.com/users/" <> organisation
                         simpleOwnerAvatarUrl = Github.URL "https://avatars1.githubusercontent.com/u/5656336?v=4"
                         simpleOwnerType      = Github.OwnerOrganization
                     in Github.SimpleOwner {..}


genesis :: Map.Map Text.Text Text.Text
genesis = Map.fromList
  [ ("fpco", "monad-unlift")
  , ("Gabriel439", "Haskell-MMorph-Library")
  , ("mvv", "transformers-base")
  , ("basvandijk", "monad-control")
  ]

git :: String -> String -> ProcessConfig () () ()
git organisationName repoName = proc "git" ["clone", "git@github.com:" <> organisationName <> "/" <> repoName <> ".git"]

makeRepoSshUrl, makeRepoHtmlUrl, makeRepoUrl, makeRepoHooksUrl :: Text.Text -> Text.Text -> Github.URL
makeRepoSshUrl   organisation repoName = Github.URL $ "git@github.com:" <> organisation <> "/" <> repoName <> ".git"
makeRepoHtmlUrl  organisation repoName = Github.URL $ "https://github.com/" <> organisation <> "/" <> repoName
makeRepoUrl      organisation repoName = Github.URL $ "https://api.github.com/repos/" <> organisation <> "/" <> repoName
makeRepoHooksUrl organisation repoName = Github.URL (Github.getUrl (makeRepoUrl organisation repoName) <> "/hooks")

repos2 :: Vector.Vector Github.Repo
repos2 = Vector.fromList $ uncurry mkRepo <$> Map.toList genesis

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
