module SourceFetch.Init.SshUrlParser (
  parseSshUrl
  ) where

import qualified GitHub.Data                              as Github (URL, getUrl)
import           SourceFetch.Init.Data                    (OrganisationName (..), RepoName (..))
import           Text.Parsec                              (ParseError, parse)
import           Text.Parsec.Text                         (Parser)
import           Text.ParserCombinators.Parsec.Char       (satisfy, string)
import           Text.ParserCombinators.Parsec.Combinator (many1)

parseSshUrl :: Github.URL -> Either ParseError (OrganisationName, RepoName)
parseSshUrl parsee = parse orgNameAndRepoName "repo-parser" (Github.getUrl parsee)
  where

    orgNameAndRepoName :: Parser (OrganisationName, RepoName)
    orgNameAndRepoName = do
       org  <- organisationParser
       name <- repoParser
       pure (org, name)

    repoParser :: Parser RepoName
    repoParser = string "/" *> (RepoName <$> many1 (satisfy ('.' /=))) <* string ".git"

    organisationParser :: Parser OrganisationName
    organisationParser = string "git@github.com:" *> (OrganisationName <$> many1 (satisfy ('/' /=)))
