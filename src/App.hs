module App (App(..), appToHandler) where

import Control.Monad.Identity (IdentityT (..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Catch (MonadThrow, try)
import Servant (Handler(..))
import Control.Monad.Except (ExceptT(..))

newtype App a = App (IdentityT IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow)

appToHandler :: App a -> Handler a
appToHandler (App app) = (Handler . ExceptT . try . runIdentityT) app
