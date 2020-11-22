{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric, DuplicateRecordFields #-}
module Main where

import Control.Monad (msum, when)
import Crypto.KDF.BCrypt (validatePassword)
import qualified Data.ByteArray.Encoding as BE
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Pool (createPool, withResource)
import Data.Maybe (fromMaybe, isNothing)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple
--import qualified Database.SQLite.Simple as SQLite
import Network.Wai.Application.Static (staticApp, defaultWebAppSettings)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Handler.Warp (run)
import Options.Applicative
import System.Environment (getEnvironment)
import System.Exit (die)
import qualified Web.Routing.Combinators as R
import qualified Web.Routing.SafeRouting as R

import qualified PostgreSQL as BP
import Server
import Server.Config
import Server.Internal
import Server.Session (defaultKey, defaultLoadSession)
import GHC.Generics
import Data.Yaml (decodeFile, FromJSON(..), ToJSON(..))
import Data.Yaml.Config
import Data.Aeson.Casing
import Data.Aeson (genericToJSON, genericParseJSON)

data AppConfig = AppConfig { webappsDir :: FilePath
                           , port :: Int
                           , database :: DBConfig
                           } deriving (Generic, Read, Show)

data DBConfig = DBConfig {  user :: String
                         , password :: String
                         , host :: String
                         , port :: Int
                         , database :: String
                         } deriving (Generic, Read, Show)

instance FromJSON AppConfig
instance FromJSON DBConfig

getPostgresConnexionString :: DBConfig -> String
getPostgresConnexionString DBConfig {..} = concat ["postgres://", user, ":", password, "@", host, ":", show port, "/", database]

main :: IO ()
main = do
--    maybeAppConfigs <- decodeFile "config/settings.yml" :: IO (Maybe AppConfig)
    appConfigs <- loadYamlSettings ["config/settings.yml"] [] useEnv :: IO AppConfig
--    let Just appConfigs = maybeAppConfigs
    putStrLn (show appConfigs)
    env <- getEnvironment
    sidSalt <- decodeSalt $ lookup "SUBJECT_ID_SALT" env
    when (isNothing sidSalt) $ putStrLn "Subject identifiers will be shared between clients. Set SUBJECT_ID_SALT to use pairwise identifiers)"
    runWithOptions appConfigs sidSalt

decodeSalt :: Maybe String -> IO (Maybe ByteString)
decodeSalt Nothing = return Nothing
decodeSalt (Just s) = case bs of
    Left  _ -> die "salt value must be hex or base64 encoded"
    Right b -> return (Just b)
  where
    bs = let b = BC.pack s in msum [BE.convertFromBase BE.Base64 b, BE.convertFromBase BE.Base16 b]

runWithOptions :: AppConfig -> Maybe ByteString -> IO ()
runWithOptions AppConfig {..} sidSalt = do
    sessionKey <- defaultKey
    kr <- defaultKeyRing
    rotateKeys kr True
    (mkBackEnd, passwordAuthenticate) <- do
                                          pool <- createPool (connectPostgreSQL (BC.pack $ getPostgresConnexionString database)) close 1 60 20
                                          return (BP.postgreSQLBackend pool, BP.passwordAuthenticate pool)
    config <- mkBackEnd <$> inMemoryConfig "" kr sidSalt
    let app = staticApp (defaultWebAppSettings "webroot")
        baseRouter = authServer config defaultApprovalPage authenticatedSubject authenticateSubject
        authenticate username password = passwordAuthenticate validatePassword username (TE.encodeUtf8 password)
        extraRoutes =
            [ ("/home",   text "Hello, I'm the home page")
            , ("/login",  passwordLoginHandler defaultLoginPage authenticate)
            , ("/logout", invalidateSession >> text "You have been logged out")
            ]
        router = foldl (\pathMap (r, h) -> R.insertPathMap' (R.toInternalPath (R.static r)) (const h) pathMap) baseRouter extraRoutes
        authApp = routerToMiddleware (defaultLoadSession 3600 sessionKey) router

    run port (logStdoutDev (authApp app))
