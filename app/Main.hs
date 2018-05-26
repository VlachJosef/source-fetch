module Main where

import           Control.Monad                (forever)
import           Control.Monad.Trans.Identity (runIdentityT)
import           SourceFetch.Completion       (executeCmd)

main :: IO ()
main = forever $ runIdentityT executeCmd
