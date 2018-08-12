module SourceFetch where

import           Common                     (toExceptT, toExceptTM)
import           Control.Monad              (when)
import           Control.Monad.Error.Class  (throwError)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except (ExceptT (..))
import           Control.Newtype.Generics   (op, under)
import           Data.Bifunctor             (first)
import           Data.ByteString            (ByteString, readFile)
import           Data.Either                (partitionEithers)
import           Data.Functor.Compose       (Compose (..))
import           Data.Semigroup             ((<>))
import           Data.Text                  (Text, lines, pack, unpack)
import           Data.Text.Encoding         (decodeUtf8)
import qualified GitHub.Data                as Github (OwnerType (..), Repo (..), SimpleOwner (..), URL (..), getUrl,
                                                       mkOwnerId, mkOwnerName, mkRepoId, mkRepoName)
import           Prelude                    hiding (lines, readFile)
import           SourceFetch.FakeRepos      (genesis, mkRepo)
import           SourceFetch.Init.Data
import           Text.Parsec                (ParseError, anyChar, many1, noneOf, parse, spaces)
import           Text.Parsec.Error          (Message (..), addErrorMessage)
import           Text.Parsec.Text           (Parser)

toGithubRepo :: RepoSource -> Github.Repo
toGithubRepo (RepoSource (OrganisationName on) (RepoName rn)) = mkRepo on rn

pRepoSource :: Parser RepoSource
pRepoSource = RepoSource <$> pOrganisationName <* spaces <*> pRepoName

pOrganisationName :: Parser OrganisationName
pOrganisationName = OrganisationName . pack <$> many1 (noneOf " ")

pRepoName :: Parser RepoName
pRepoName = RepoName . pack <$> many1 anyChar

readTest :: ExceptT InitError IO [Github.Repo]
readTest = do
  toExceptTM PError $ mkGithubRepo <$> readFile "scripts/test.txt"
  where
    mkGithubRepo :: ByteString -> Either ParseError [Github.Repo]
    mkGithubRepo = under Compose ((<$>) toGithubRepo) . traverse toRepoSource . lines . decodeUtf8

toRepoSource :: Text -> Either ParseError RepoSource
toRepoSource t = first (addErrorMessage  (Message . unpack $ "Error parsing: " <> t)) (parse pRepoSource "launch-command" t)
