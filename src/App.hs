module App (App(..), appToHandler, makeAppState) where

import Control.Concurrent.STM
  (TVar, atomically, modifyTVar, newTVarIO, readTVarIO)
import Control.Monad.Catch (MonadThrow, try)
import Control.Monad.Except (ExceptT(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT(runReaderT), asks)
import Data.ByteString.UTF8 (ByteString)
import Data.Function ((&))
import Servant (Handler(..))
import Auth (Blacklist(..))

newtype AppState = AppState {blacklist :: TVar [ByteString]}

newtype App a = App (ReaderT AppState IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadReader AppState)

appToHandler :: AppState -> App a -> Handler a
appToHandler state (App rt) = Handler . ExceptT . try $ runReaderT rt state

makeAppState :: IO AppState
makeAppState = do
  blacklist <- newTVarIO []
  pure AppState {blacklist}

instance Blacklist App where
  isBlacklisted :: ByteString -> App Bool
  isBlacklisted token = do
    tokens <- asks blacklist
    elem token <$> readTVarIO tokens
      & liftIO

  addToBlacklist :: ByteString -> App ()
  addToBlacklist token = do
    tokens <- asks blacklist
    atomically (modifyTVar tokens (token:))
      & liftIO
