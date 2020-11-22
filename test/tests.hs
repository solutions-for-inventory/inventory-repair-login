{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Data.Aeson (encode)
import Jose.Jwk
import Network.HTTP.Types
import Network.Wai
import qualified Network.Wai.Handler.Warp as W
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Test.Hspec (hspec)

import qualified Auth.Server.Session as Session
import Auth.Test
import Auth.Server.Internal
import Auth.OAuth2.TestData (clientPublicJwks)
import qualified Auth.OAuth2.TokenSpec as TokenSpec
import qualified Auth.OAuth2.AuthorizationSpec as AuthorizationSpec
import qualified OAuth2IntegrationSpec
import qualified OICIntegrationSpec
import WaiTest

keyServer :: Application
keyServer _ r = r $ responseLBS status200 [("Content-Type", "application/json")] (encode (JwkSet clientPublicJwks))

main :: IO ()
main = do
    tid <- forkIO $ W.runSettings (W.setPort 60129 $ W.setHost "0.0.0.0" W.defaultSettings) keyServer
    app <- testapp
    let run t = runTest app t
    hspec $ do
        TokenSpec.spec
        AuthorizationSpec.spec
        (OAuth2IntegrationSpec.spec run)
        (OICIntegrationSpec.spec run)
    killThread tid

testapp = do
    let issuer = "http://testapp"
    csKey <- Session.defaultKey
    router <- testAuth issuer
    let app = routerToApp (Session.defaultLoadSession 60 csKey) issuer router
    return $ logStdoutDev app
