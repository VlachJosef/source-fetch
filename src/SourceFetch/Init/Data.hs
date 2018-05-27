module SourceFetch.Init.Data
  ( RepoName(..)
  , OrganisationName(..)
  ) where

newtype RepoName = RepoName
  { unRepoName :: String
  }

newtype OrganisationName = OrganisationName
  { unOrganisationName :: String
  }
