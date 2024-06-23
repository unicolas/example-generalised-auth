{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Auth (authHandler, signToken, generateKey, Blacklist(..), pass, revoke) where

import Control.Applicative (liftA2)
import Control.Arrow (second, (>>>))
import Control.Monad (guard, unless)
import Control.Monad.IO.Class (liftIO)
import Crypto.JOSE
  ( JWK
  , KeyMaterialGenParam(OctGenParam)
  , bestJWSAlg
  , decodeCompact
  , genJWK
  , newJWSHeader
  , runJOSE
  )
import Crypto.JWT
  (HasClaimsSet, JWTError, JWTValidationSettings, SignedJWT, signJWT, verifyJWT)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString.Lazy.UTF8 as LazyByteString
import Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.UTF8 as ByteString
import Network.Wai (Request, requestHeaders)
import Servant (Handler(..))
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)

authHandler :: (HasClaimsSet a, FromJSON a)
  => JWK
  -> JWTValidationSettings
  -> (ByteString -> m Bool)
  -> (forall b. m b -> Handler b)
  -> AuthHandler Request (Maybe a)
authHandler jwk settings accept nt = mkAuthHandler $ \case
  (getToken -> Just token) -> liftA2 (<*)
    (verifyToken jwk settings token)
    (runAccept accept nt token)
  _ -> pure Nothing

getToken :: Request -> Maybe ByteString
getToken req = do
  (scheme, token) <- split <$> lookup "Authorization" (requestHeaders req)
  guard (scheme == "Bearer")
  pure token
  where
    split = ByteString.break (== ' ') >>> second (ByteString.drop 1)

runAccept :: (ByteString -> m Bool)
  -> (forall a. m a -> Handler a)
  -> ByteString
  -> Handler (Maybe ())
runAccept accept nt = fmap guard . nt . accept

verifyToken :: (HasClaimsSet a, FromJSON a)
  => JWK
  -> JWTValidationSettings
  -> ByteString
  -> Handler (Maybe a)
verifyToken jwk settings token = liftIO (maybeRight <$> runJOSE @JWTError verify)
  where
    verify = decode token >>= verifyJWT settings jwk
    decode = ByteString.toString >>> LazyByteString.fromString >>> decodeCompact

signToken :: (ToJSON a) => JWK -> a -> IO (Maybe SignedJWT)
signToken jwk claims = maybeRight <$> runJOSE @JWTError sign
  where
    sign = do
      alg <- bestJWSAlg jwk
      signJWT jwk (newJWSHeader ((), alg)) claims

generateKey :: IO JWK
generateKey = genJWK (OctGenParam 256)

maybeRight :: Either a b -> Maybe b
maybeRight = either (const Nothing) Just

class Monad m => Blacklist m where
  isBlacklisted :: ByteString -> m Bool
  addToBlacklist :: ByteString -> m ()

-- | Accepts and revokes a token if not revoked already, rejects it otherwise.
revoke :: Blacklist m => ByteString -> m Bool
revoke token = do
  blacklisted <- isBlacklisted token
  unless blacklisted (addToBlacklist token)
  pure (not blacklisted)

-- | Always accepts a token.
pass :: Applicative f => ByteString -> f Bool
pass _ = pure True
