module DotGetBlame.ExploreMMorph where

import           Control.Monad.Base

import           Control.Applicative       (Const (..))
import           Control.Monad.Morph
import           Control.Monad.Reader
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State
import           Control.Monad.Writer
import           Data.Functor.Identity

test :: String
test = "BLAME"
