{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Api (api, Api) where

import App (App)
import AuthClaims (AccessClaims, RefreshClaims)
import Control.Exception (throw)
import Crypto.JWT (JWK)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Handlers
  ( LoginRequest
  , LoginResponse
  , getUserHandler
  , loginHandler
  , refreshTokenHandler
  )
import Servant
  ( AuthProtect
  , Capture
  , Get
  , JSON
  , NamedRoutes
  , Post
  , ReqBody
  , err401
  , type (:>)
  )
import Servant.API.Generic (type (:-))
import Servant.Server.Experimental.Auth (AuthServerData)
import Servant.Server.Generic (AsServerT)
import User (User(..))

type Json = '[JSON]

type AuthJwtAccess = AuthProtect "jwt-access"
type AuthJwtRefresh = AuthProtect "jwt-refresh"

type instance AuthServerData AuthJwtAccess = Maybe AccessClaims
type instance AuthServerData AuthJwtRefresh = Maybe RefreshClaims

data Api mode = Api
  { login :: mode
      -- POST /login
      :- "login"
      :> ReqBody Json LoginRequest
      :> Post Json LoginResponse
  , refresh :: mode
      -- POST /refresh
      :- "refresh"
      :> AuthJwtRefresh
      :> Post Json LoginResponse
  , secured :: mode
      :- AuthJwtAccess
      :> NamedRoutes SecuredRoutes
  }
  deriving Generic

api :: JWK -> Api (AsServerT App)
api jwk = Api
  { login = loginHandler jwk
  , refresh = refreshTokenHandler jwk
  , secured = securedHandlers
  }

newtype SecuredRoutes mode = SecuredRoutes
  { getUser :: mode
      -- GET /users/:userId
      :- "users"
      :> Capture "userId" UUID
      :> Get Json User
  }
  deriving Generic

securedHandlers :: Maybe AccessClaims -> SecuredRoutes (AsServerT App)
securedHandlers (Just _) = SecuredRoutes { getUser = getUserHandler }
securedHandlers _ = throw err401
