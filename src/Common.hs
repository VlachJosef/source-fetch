module Common
  ( getAuth
  , goToClonesDir
  ) where

import           Control.Monad.Extra     (unlessM)
import qualified GitHub                  (Auth (..))
import           GitHub.Internal.Prelude (fromString)
import           System.Directory        (createDirectory, doesDirectoryExist, getCurrentDirectory)
import           System.Environment      (lookupEnv)
import           System.FilePath         ((</>))

getAuth :: IO (Maybe GitHub.Auth)
getAuth = do
    token <- lookupEnv "GITHUB_TOKEN"
    pure (GitHub.OAuth . fromString <$> token)

goToClonesDir :: IO FilePath
goToClonesDir = do
  clonesDir <- (</> "clones") <$> getCurrentDirectory

  clonesDir <$ unlessM
    (doesDirectoryExist clonesDir)
    (createDirectory clonesDir)
