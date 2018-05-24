{-# LANGUAGE NoImplicitPrelude #-}
module Common (
    getAuth,
    ) where

import qualified GitHub
import           GitHub.Internal.Prelude
import           System.Environment      (lookupEnv)

getAuth :: IO (Maybe (GitHub.Auth))
getAuth = do
    token <- lookupEnv "GITHUB_TOKEN"
    pure (GitHub.OAuth . fromString <$> token)
