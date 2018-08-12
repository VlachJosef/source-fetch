{-# LANGUAGE DeriveGeneric #-}
module SourceFetch.Init.Data
  ( InitError (..)
  , OrganisationName (..)
  , RepoSource (..)
  , RepoName (..)
  ) where

import           Control.Newtype.Generics
import           Data.Text
import           GHC.Generics             (Generic)
import qualified GitHub.Data              as Github (Error)
import           Text.Parsec              (ParseError)

data RepoSource = RepoSource
  { rsOrganisation :: OrganisationName
  , rsName         :: RepoName
  } deriving Show

newtype RepoName = RepoName
  { unRepoName :: Text
  } deriving (Generic, Show)

newtype OrganisationName = OrganisationName
  { unOrganisationName :: Text
  } deriving (Generic, Show)

instance Newtype OrganisationName
instance Newtype RepoName

data InitError
  = GError Github.Error
  | PError ParseError
  deriving Show
