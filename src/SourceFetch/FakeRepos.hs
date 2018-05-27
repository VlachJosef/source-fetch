{-# LANGUAGE RecordWildCards #-}

module SourceFetch.FakeRepos (
  genesis
  , mkRepo
  ) where

import qualified Data.Map.Strict as Map (Map, fromList)
import           Data.Semigroup  ((<>))
import           Data.Text       (Text)
import qualified GitHub.Data     as Github (OwnerType (..), Repo (..), SimpleOwner (..), URL (..), getUrl, mkOwnerId,
                                            mkOwnerName, mkRepoId, mkRepoName)

genesis :: Map.Map Text Text
genesis = Map.fromList
  [ ("fpco", "monad-unlift")
  , ("Gabriel439", "Haskell-MMorph-Library")
  , ("mvv", "transformers-base")
  , ("basvandijk", "monad-control")
  ]

owner :: Text -> Github.SimpleOwner
owner organisation = let simpleOwnerId        = Github.mkOwnerId 1
                         simpleOwnerLogin     = Github.mkOwnerName organisation
                         simpleOwnerUrl       = Github.URL $ "https://api.github.com/users/" <> organisation
                         simpleOwnerAvatarUrl = Github.URL "https://avatars1.githubusercontent.com/u/5656336?v=4"
                         simpleOwnerType      = Github.OwnerOrganization
                     in Github.SimpleOwner {..}

makeRepoSshUrl, makeRepoHtmlUrl, makeRepoUrl, makeRepoHooksUrl :: Text -> Text -> Github.URL
makeRepoSshUrl   organisation repoName = Github.URL $ "git@github.com:" <> organisation <> "/" <> repoName <> ".git"
makeRepoHtmlUrl  organisation repoName = Github.URL $ "https://github.com/" <> organisation <> "/" <> repoName
makeRepoUrl      organisation repoName = Github.URL $ "https://api.github.com/repos/" <> organisation <> "/" <> repoName
makeRepoHooksUrl organisation repoName = Github.URL (Github.getUrl (makeRepoUrl organisation repoName) <> "/hooks")

mkRepo :: Text -> Text -> Github.Repo
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
