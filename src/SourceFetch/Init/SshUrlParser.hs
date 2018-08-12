module SourceFetch.Init.SshUrlParser
  ( parseSshUrl
  ) where

import           Common.Parser         (pNotContaining)
import qualified GitHub.Data           as Github (URL, getUrl)
import           SourceFetch.Init.Data (OrganisationName (..), RepoName (..))
import           Text.Parsec           (ParseError, parse)
import           Text.Parsec.Text      (Parser)


parseSshUrl :: Github.URL -> Either ParseError (OrganisationName, RepoName)
parseSshUrl parsee = parse orgNameAndRepoName "repo-parser" (Github.getUrl parsee)
  where
    orgNameAndRepoName :: Parser (OrganisationName, RepoName)
    orgNameAndRepoName = (,) <$> pOrganisationName <*> pRepoName

    pRepoName :: Parser RepoName
    pRepoName = pNotContaining RepoName '.'

    pOrganisationName :: Parser OrganisationName
    pOrganisationName = pNotContaining OrganisationName '/'
