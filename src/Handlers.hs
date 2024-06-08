{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Handlers
  ( LoginRequest(..)
  , LoginResponse(..)
  , getUserHandler
  , loginHandler
  , refreshTokenHandler
  ) where

import Auth (signToken)
import AuthClaims (RefreshClaims, accessClaims, refreshClaims, subjectClaim)
import Control.Monad (unless, when)
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Crypto.JOSE (JWK, encodeCompact)
import Crypto.JWT (SignedJWT)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Lazy.UTF8 (toString)
import Data.Text (Text, pack)
import Data.Time (getCurrentTime)
import Data.UUID (UUID, nil)
import GHC.Generics (Generic)
import Servant (ServerError(errBody), err401, err404, err500)
import User (User(..))

data LoginRequest = LoginRequest
  { username :: !Text
  , password :: !Text
  }
  deriving (Generic, FromJSON)

data LoginResponse = LoginResponse
  { access :: !Text
  , refresh :: !Text
  }
  deriving (Generic, ToJSON)

makeLoginResponse :: MonadThrow m => [Maybe SignedJWT] -> m LoginResponse
makeLoginResponse = \case
  [Just (toText -> access), Just (toText -> refresh)]
    -> pure LoginResponse {access, refresh}
  _ -> throwM err500 {errBody = "Failed to generate new tokens"}
  where
    toText = pack . toString . encodeCompact

loginHandler :: MonadIO m => JWK -> LoginRequest -> m LoginResponse
loginHandler jwk LoginRequest {username, password} = liftIO $ do
  unless (username == "user" && password == "12345") (throwM err401)
  now <- getCurrentTime
  signedAccess <- signToken jwk (accessClaims nil now)
  signedRefresh <- signToken jwk (refreshClaims nil now)
  makeLoginResponse [signedAccess, signedRefresh]

refreshTokenHandler :: (MonadThrow m, MonadIO m)
  => JWK -> Maybe RefreshClaims -> m LoginResponse
refreshTokenHandler jwk (Just claims@(subjectClaim -> Just uid)) = liftIO $ do
  now <- getCurrentTime
  signedAccess <- signToken jwk (accessClaims uid now)
  signedRefresh <- signToken jwk claims
  makeLoginResponse [signedAccess, signedRefresh]
refreshTokenHandler _ _ = throwM err401

getUserHandler :: MonadThrow m => UUID -> m User
getUserHandler uid = do
  when (uid /= nil) (throwM err404)
  pure (User "user" "user@mail.com")
