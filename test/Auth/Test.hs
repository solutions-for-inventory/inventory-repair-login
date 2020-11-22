{-# LANGUAGE OverloadedStrings #-}

module Test where

import           Crypto.KDF.BCrypt (validatePassword)
import           Data.Pool (createPool, withResource)
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import           Database.SQLite.Simple as SQLite
import qualified Web.Routing.Combinators as R
import qualified Web.Routing.SafeRouting as R

import           Model
import           Server (authServer, defaultLoginPage,  defaultApprovalPage, authenticatedSubject, authenticateSubject, passwordLoginHandler)
import           Server.Internal
import           Server.Config
import           SQLite (createSchema, sqliteBackend, passwordAuthenticate)
import           Test.Data

testAuth :: Text -> IO (R.PathMap (Handler ()))
testAuth issuer = do
    pool <- createPool (SQLite.open "file::memory:?cache=shared") SQLite.close 1 60 20
    withResource pool $ \c -> createSchema c
    kr <- defaultKeyRing
    config <- sqliteBackend pool <$> inMemoryConfig issuer kr Nothing
    mapM_ (createClient config) testClients
    rotateKeys kr True
    -- Allow everything for test options
    let testConfig = config { responseTypesSupported = map snd responseTypes }
        passAuth v u p = withResource pool $ \c -> passwordAuthenticate c v u p

        authenticate username password = passAuth validatePassword username (encodeUtf8 password)
        extraRoutes =
            [ ("/home",   text "Hello, I'm the home page")
            , ("/login",  passwordLoginHandler defaultLoginPage authenticate)
            , ("/logout", invalidateSession >> complete)
            ]
        routingTable = foldl (\pathMap (r, h) -> R.insertPathMap' (R.toInternalPath (R.static r)) (const h) pathMap) (authServer testConfig defaultApprovalPage authenticatedSubject authenticateSubject) extraRoutes
    return routingTable
