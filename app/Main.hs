module Main (main) where

import Api (api)
import App (appToHandler)
import Auth (authHandler, generateKey)
import AuthClaims (AccessClaims, RefreshClaims, accessSettings, refreshSettings)
import Network.Wai.Handler.Warp (run)
import Servant (Context(..))
import Servant.Server.Generic (genericServeTWithContext)

main :: IO ()
main = do
  jwk <- generateKey
  let
    port = 8080
    ctx = authHandler @AccessClaims jwk accessSettings
      :. authHandler @RefreshClaims jwk refreshSettings
      :. EmptyContext
    app = genericServeTWithContext appToHandler (api jwk) ctx
  run port app
