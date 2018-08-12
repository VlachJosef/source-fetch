module Common.Parser
  ( pNotContaining
  ) where

import           Data.Text
import           SourceFetch.Init.Data
import           Text.Parsec.Char       (satisfy)
import           Text.Parsec.Combinator (many1)
import           Text.Parsec.Text       (Parser)

import           Data.Bifunctor
import           Data.Functor.Compose

pNotContaining :: (Text -> b) -> Char -> Parser b
pNotContaining f c = f . pack <$> many1 (satisfy (c /=))
