module Main (main) where

import Api (api)
import App (appToHandler, makeAppState)
import Auth (authHandler, generateKey, revoke, pass)
import AuthClaims (AccessClaims, RefreshClaims, accessSettings, refreshSettings)
import Network.Wai.Handler.Warp (run)
import Servant (Context(..))
import Servant.Server.Generic (genericServeTWithContext)

main :: IO ()
main = do
  jwk <- generateKey
  appState <- makeAppState
  let
    port = 8080
    nt = appToHandler appState
    ctx = authHandler @AccessClaims jwk accessSettings pass nt
      :. authHandler @RefreshClaims jwk refreshSettings revoke nt
      :. EmptyContext
    app = genericServeTWithContext nt (api jwk) ctx
  run port app
