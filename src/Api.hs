{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Api (api, Api) where

import AuthClaims (AccessClaims, RefreshClaims)
import Crypto.JWT (JWK)
import GHC.Generics (Generic)
import Handlers
  ( LoginRequest
  , LoginResponse
  , getUserHandler
  , loginHandler
  , refreshTokenHandler
  )
import qualified Servant as Http (Get, Post)
import Servant
  ( AuthProtect
  , Capture
  , JSON
  , NamedRoutes
  , ReqBody
  , type (:>)
  , err401
  )
import Servant.API.Generic (type (:-))
import Servant.Server.Experimental.Auth (AuthServerData)
import Servant.Server.Generic (AsServerT)
import User (User(..))
import Data.UUID (UUID)
import App (App)
import Control.Exception (throw)

type Json = '[JSON]

type AuthJwtAccess = AuthProtect "jwt-access'"
type AuthJwtRefresh = AuthProtect "jwt-refresh'"

type instance AuthServerData AuthJwtAccess = Maybe AccessClaims
type instance AuthServerData AuthJwtRefresh = Maybe RefreshClaims

data Api mode = Api
  { login :: mode
      -- POST /login
      :- "login"
      :> ReqBody Json LoginRequest
      :> Http.Post Json LoginResponse
  , refresh :: mode
      -- POST /refresh
      :- "refresh"
      :> AuthJwtRefresh
      :> Http.Post Json LoginResponse
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
      :> Http.Get Json User
  }
  deriving Generic

securedHandlers :: Maybe AccessClaims -> SecuredRoutes (AsServerT App)
securedHandlers (Just _) = SecuredRoutes { getUser = getUserHandler }
securedHandlers _ = throw err401
