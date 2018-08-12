module Common
  ( getAuth
  , goToClonesDir
  , ifDirExists
  , ifDirExistsT
  , ifDirExists_
  , ifDirExistsT_
  , toExceptT
  , toExceptTM
  ) where

import           Control.Monad.Extra        (unlessM)
import           Control.Monad.Reader       (ReaderT)
import           Control.Monad.Trans.Class  (MonadTrans, lift)
import           Control.Monad.Trans.Except (ExceptT (..), withExceptT)
import           Data.Functor               (void)
import qualified GitHub                     (Auth (..))
import           GitHub.Internal.Prelude    (fromString)
import           System.Directory           (createDirectory, doesDirectoryExist, getCurrentDirectory)
import           System.Environment         (lookupEnv)
import           System.FilePath            ((</>))

getAuth :: IO (Maybe GitHub.Auth)
getAuth = do
    token <- lookupEnv "GITHUB_TOKEN"
    pure (GitHub.OAuth . fromString <$> token)

predicateT :: (MonadTrans t, Monad m, Monad (t m)) => (a -> m Bool) -> a -> m b -> m b -> t m b
predicateT predic value onTrue onFalse = lift $ predicate predic value onTrue onFalse

predicateT_ :: (MonadTrans t, Monad m, Monad (t m)) => (a -> m Bool) -> a -> m b -> m c -> t m ()
predicateT_ predic value onTrue onFalse = lift $ predicate_ predic value onTrue onFalse

predicate :: (Monad m) => (a -> m Bool) -> a -> m b -> m b -> m b
predicate predic value onTrue onFalse = do
  success <- predic value
  if success
    then onTrue
    else onFalse

predicate_ :: (Monad m) => (a -> m Bool) -> a -> m b -> m c -> m ()
predicate_ predic value onTrue onFalse = do
  success <- predic value
  if success
    then void onTrue
    else void onFalse

ifDirExists :: FilePath -> IO a -> IO a -> IO a
ifDirExists = predicate doesDirectoryExist

ifDirExists_ :: FilePath -> IO a -> IO b -> IO ()
ifDirExists_ = predicate_ doesDirectoryExist

ifDirExistsT :: FilePath -> IO a -> IO a -> ReaderT c IO a
ifDirExistsT = predicateT doesDirectoryExist

ifDirExistsT_ :: FilePath -> IO a -> IO b -> ReaderT c IO ()
ifDirExistsT_ = predicateT_ doesDirectoryExist

goToClonesDir :: IO FilePath
goToClonesDir = do
  clonesDir <- (</> "clones") <$> getCurrentDirectory

  clonesDir <$ unlessM
    (doesDirectoryExist clonesDir)
    (createDirectory clonesDir)

toExceptT :: Monad m => (a -> b) -> Either a c -> ExceptT b m c
toExceptT f e = toExceptTM f (pure e)

toExceptTM :: Monad m => (a -> b) -> m (Either a c) -> ExceptT b m c
toExceptTM f e = withExceptT f $ ExceptT e
