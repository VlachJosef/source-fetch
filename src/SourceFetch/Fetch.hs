{-# LANGUAGE RecordWildCards #-}
module SourceFetch.Fetch
  ( execFetch
  , execPull
  )  where

import           Control.Monad.Reader       (ReaderT, withReaderT)
import           Control.Monad.Trans.Except (runExceptT)
import           Data.Functor               (void)
import           Data.Semigroup             ((<>))
import           SourceFetch.Init           (lockDotGit, unlockDotGit)
import           SourceFetch.Init.Data      (InitError (..), RepoSource (..))
import           SourceFetch.Types          (Env (..), EnvWithProcess (..), EnvWithRepo (..), RepoLevel)
import           SourceFetch.Util           (execProcess, runAction)
import           System.Process.Typed       (ProcessConfig, proc)

execFetch, execPull :: IO ()
execFetch = execGit gitFetch
execPull  = execGit gitPull

gitFetch :: ProcessConfig () () ()
gitFetch = proc "git" ["fetch", "-v"]

gitPull :: ProcessConfig () () ()
gitPull = proc "git" ["pull", "-v"]

execGit :: ProcessConfig () () () -> IO ()
execGit pc = do
  result <- runExceptT $ runAction $ feed
  case result of
    Left (GError githubError) -> putStrLn $ "GithubError " <> show githubError
    Left (PError parseError)  -> putStrLn $ "ParseError " <> show parseError
    Right _                   -> pure ()
  where
   feed :: [RepoSource] -> ReaderT Env IO ()
   feed xs = void $ traverse (processRepo pc) xs

processRepo :: ProcessConfig () () () -> RepoSource -> ReaderT Env IO ()
processRepo pc rs = withReaderT mkEwp withUnlockedRepo
  where
    mkEwp :: Env -> (EnvWithProcess RepoLevel)
    mkEwp env = EnvWithProcess (EnvWithRepo env rs) (const pc)

withUnlockedRepo :: ReaderT (EnvWithProcess RepoLevel) IO ()
withUnlockedRepo =
  unlockDotGit *>
  execProcess *>
  lockDotGit
