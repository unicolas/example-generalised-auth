{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers 
  ( LoginRequest(..)
  , LoginResponse(..)
  , getUserHandler
  , loginHandler
  , refreshTokenHandler
  ) where

import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Servant (err401, err404)
import Crypto.JOSE (JWK, encodeCompact)
import AuthClaims (RefreshClaims, accessClaims, refreshClaims, subjectClaim)
import Data.UUID (nil, UUID)
import Control.Monad (when, unless)
import Data.Time (getCurrentTime)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Auth (signToken)
import Data.ByteString.Lazy.UTF8 (toString)
import User (User(..))
import Control.Monad.Catch (MonadThrow(..))
  
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

loginResponse :: (ToJSON a, ToJSON b, MonadThrow m, MonadIO m) 
  => JWK -> a -> b -> m LoginResponse
loginResponse jwk acc refr = do
  signedAccess <- liftIO (signToken jwk acc)
  signedRefresh <- liftIO (signToken jwk refr)
  case (signedAccess, signedRefresh) of
    (Just (toText -> access), Just (toText -> refresh)) -> pure LoginResponse {..}
    _ -> throwM err401
  where
    toText = pack . toString . encodeCompact

loginHandler :: (MonadThrow m, MonadIO m) 
  => JWK -> LoginRequest -> m LoginResponse
loginHandler jwk LoginRequest {..} = do
  unless (username == "user" && password == "12345") (throwM err401)
  do
    now <- liftIO getCurrentTime
    loginResponse jwk (accessClaims nil now) (refreshClaims nil now)

refreshTokenHandler :: (MonadThrow m, MonadIO m)
  => JWK -> Maybe RefreshClaims -> m LoginResponse
refreshTokenHandler jwk (Just claims@(subjectClaim -> Just uid)) = do
  now <- liftIO getCurrentTime
  loginResponse jwk (accessClaims uid now) claims
refreshTokenHandler _ _ = throwM err401

getUserHandler :: MonadThrow m => UUID -> m User
getUserHandler uid = do
  when (uid /= nil) (throwM err404)
  pure (User "user" "user@mail.com")
