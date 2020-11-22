{-# LANGUAGE OverloadedStrings, FlexibleContexts, QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Auth.OAuth2.TestData where

import Data.Aeson
import Data.Aeson.QQ
import Data.Time.Clock.POSIX
import Jose.Jwt (KeyId(..), IntDate(..))
import Jose.Jwk

import Model
import URI

-- $ date -r 1400000000
-- Tue 13 May 2014 17:53:20 BST
now = fromIntegral (1400000000 :: Int) :: POSIXTime


-- Client JWKs

publicKeySet :: Object
Object publicKeySet = [aesonQQ|
    { "keys" :
        [ {"kty":"EC", "crv":"P-256", "x":"MKBCTNIcKUSDii11ySs3526iDZ8AiTo7Tu6KPAqv7D4", "y":"4Etl6SRW2YiLUrN5vfvVHuhp7x8PxltmWWlbbM4IFyM", "use":"enc", "kid":"1"}
        , {"kty":"RSA", "n": "0vx7agoebGcQSuuPiLJXZptN9nndrQmbXEps2aiAFbWhM78LhWx4cbbfAAtVT86zwu1RK7aPFFxuhDR1L6tSoc_BJECPebWKRXjBZCiFV4n3oknjhMstn64tZ_2W-5JsGY4Hc5n9yBXArwl93lqt7_RN5w6Cf0h4QyQ5v-65YGjQR0_FDW2QvzqY368QQMicAtaSqzs8KJZgnYb9c7d0zgdAZHzu6qMQvRL5hajrn1n91CbOpbISD08qNLyrdkt-bFTWhAI4vMQFh6WeZu0fM4lFd2NcRwr3XPksINHaQ-G_xBniIqbw0Ls1jF44-csFCur-kEgU8awapJzKnqDKgw", "e":"AQAB", "alg":"RS256", "kid":"2"}
         ]
    }
|]

privateKeySet :: Object
Object privateKeySet = [aesonQQ|
    { "keys" :
        [ {"kty":"EC", "crv":"P-256", "x":"MKBCTNIcKUSDii11ySs3526iDZ8AiTo7Tu6KPAqv7D4", "y":"4Etl6SRW2YiLUrN5vfvVHuhp7x8PxltmWWlbbM4IFyM", "d":"870MB6gfuTJ4HtUnUvYMyJpr5eUZNP4Bk43bVdj3eAE", "use":"enc", "kid":"1"}
        , {"kty":"RSA", "n":"0vx7agoebGcQSuuPiLJXZptN9nndrQmbXEps2aiAFbWhM78LhWx4cbbfAAtVT86zwu1RK7aPFFxuhDR1L6tSoc_BJECPebWKRXjBZCiFV4n3oknjhMstn64tZ_2W-5JsGY4Hc5n9yBXArwl93lqt7_RN5w6Cf0h4QyQ5v-65YGjQR0_FDW2QvzqY368QQMicAtaSqzs8KJZgnYb9c7d0zgdAZHzu6qMQvRL5hajrn1n91CbOpbISD08qNLyrdkt-bFTWhAI4vMQFh6WeZu0fM4lFd2NcRwr3XPksINHaQ-G_xBniIqbw0Ls1jF44-csFCur-kEgU8awapJzKnqDKgw", "e":"AQAB", "d":"X4cTteJY_gn4FYPsXB8rdXix5vwsg1FLN5E3EaG6RJoVH-HLLKD9M7dx5oo7GURknchnrRweUkC7hT5fJLM0WbFAKNLWY2vv7B6NqXSzUvxT0_YSfqijwp3RTzlBaCxWp4doFk5N2o8Gy_nHNKroADIkJ46pRUohsXywbReAdYaMwFs9tv8d_cPVY3i07a3t8MN6TNwm0dSawm9v47UiCl3Sk5ZiG7xojPLu4sbg1U2jx4IBTNBznbJSzFHK66jT8bgkuqsk0GjskDJk19Z4qwjwbsnn4j2WBii3RL-Us2lGVkY8fkFzme1z0HbIkfz0Y6mqnOYtqc0X4jfcKoAC8Q", "p":"83i-7IvMGXoMXCskv73TKr8637FiO7Z27zv8oj6pbWUQyLPQBQxtPVnwD20R-60eTDmD2ujnMt5PoqMrm8RfmNhVWDtjjMmCMjOpSXicFHj7XOuVIYQyqVWlWEh6dN36GVZYk93N8Bc9vY41xy8B9RzzOGVQzXvNEvn7O0nVbfs", "q":"3dfOR9cuYq-0S-mkFLzgItgMEfFzB2q3hWehMuG0oCuqnb3vobLyumqjVZQO1dIrdwgTnCdpYzBcOfW5r370AFXjiWft_NGEiovonizhKpo9VVS78TzFgxkIdrecRezsZ-1kYd_s1qDbxtkDEgfAITAG9LUnADun4vIcb6yelxk", "dp":"G4sPXkc6Ya9y8oJW9_ILj4xuppu0lzi_H7VTkS8xj5SdX3coE0oimYwxIi2emTAue0UOa5dpgFGyBJ4c8tQ2VF402XRugKDTP8akYhFo5tAA77Qe_NmtuYZc3C3m3I24G2GvR5sSDxUyAN2zq8Lfn9EUms6rY3Ob8YeiKkTiBj0", "dq":"s9lAH9fggBsoFR8Oac2R_E2gw282rT2kGOAhvIllETE1efrA6huUUvMfBcMpn8lqeW6vzznYY5SSQF7pMdC_agI3nG8Ibp1BUb0JUiraRNqUfLhcQb_d9GF4Dh7e74WbRsobRonujTYN1xCaP6TO61jvWrX-L18txXw494Q_cgk", "qi":"GyM_p6JrXySiz1toFgKbWV-JdI3jQ4ypu9rbMWx3rQJBfmt0FoYzgUIZEVFEcOqwemRN81zoDAaa-Bk0KWNGDjJHZDdDmFhW3AN7lI-puxk_mHZGJ11rxyR8O55XLSe3SPmRfKwZI6yU24ZxvQKFYItdldUKGzO6Ia6zTKhAVRU", "alg":"RS256", "kid":"2"}
        ]
    }
|]

testPublicJwks :: [Jwk]
Success (JwkSet testPublicJwks) = fromJSON (Object publicKeySet)

testPrivateJwks :: [Jwk]
Success (JwkSet testPrivateJwks) = fromJSON (Object privateKeySet)

clientPublicJwks :: [Jwk]
clientPublicJwks = let RsaPublicJwk k _ _ _ = testPublicJwks !! 1
                   in  [RsaPublicJwk k (Just (KeyId "c1")) (Just Enc) Nothing]

r u = let Right uri = parseURI u in uri

-- Authorization from user "cat" to app
catAuthorization = Authorization "cat" (clientId appClient) (IntDate $ now - 20) [] Nothing (Just (r "http://app")) (now - 60)

loadAuthorization "catcode" = return $ Just catAuthorization
loadAuthorization "catoic"  = return $ Just $ catAuthorization {authzScope = [OpenID]}
loadAuthorization "expired" = return $ Just $ catAuthorization {authzAt = IntDate $ now - 301}
loadAuthorization _         = return Nothing

authenticateResourceOwner username password
    | username == password = return $ Just username
    | otherwise            = return Nothing

appClient   = Client "app" (Just "appsecret") [AuthorizationCode, RefreshToken] [r "http://app2", r "http://app"] 99 99 appClientScope False ClientSecretBasic Nothing Nothing (Just testPublicJwks) Nothing Nothing Nothing "app"
adminClient = Client "admin" (Just "adminsecret") [ClientCredentials, AuthorizationCode] [r "http://admin"] 99 99 adminClientScope False ClientSecretBasic Nothing Nothing Nothing Nothing Nothing Nothing "admin"
roClient    = Client "ro" (Just "rosecret") [ResourceOwner] [] 99 99 appClientScope False ClientSecretBasic Nothing Nothing Nothing Nothing Nothing Nothing "ro"
jsClient    = Client "js" Nothing [Implicit] [] 99 99 jsClientScope False ClientAuthNone Nothing Nothing Nothing Nothing Nothing Nothing "js"
allClient   = Client "all" (Just "allsecret") [AuthorizationCode, ClientCredentials, Implicit, ResourceOwner] [] 99 99 appClientScope False ClientSecretBasic Nothing Nothing Nothing Nothing Nothing Nothing "all"

appClientScope   = map CustomScope ["scope1", "scope2", "scope3"]
adminClientScope = appClientScope ++ [CustomScope "admin"]
jsClientScope    = map CustomScope ["weakscope"]

getClient "app"   = return $ Just appClient
getClient "appq"  = return $ Just appClient { redirectURIs = [r "http://app?x=1&y=2"] }
getClient "admin" = return $ Just adminClient
getClient "js"    = return $ Just jsClient
getClient _       = return Nothing
