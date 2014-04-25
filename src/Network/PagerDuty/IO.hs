{-# LANGUAGE OverloadedStrings #-}

module Network.PagerDuty.IO
    ( request
    )
where

import Control.Applicative
import Control.Monad.Reader
import Data.Aeson
import Data.ByteString         (ByteString)
import Data.Default
import Data.Monoid
import Network.HTTP.Client
import Network.HTTP.Types
import Network.PagerDuty.Types

import qualified Data.ByteString.Lazy as L


request :: (ToJSON a, FromJSON b)
        => Method
        -> ByteString
        -> a
        -> PagerDuty c (Either Error b)
request rqMeth rqPath rqBody = liftIO . go =<< ask
  where
    go (AuthEnv h auth mgr) = httpLbs (rq h (Just auth)) mgr >>= response
    go (Env     h      mgr) = httpLbs (rq h Nothing)     mgr >>= response

    rq h auth =
        let req = def { method         = rqMeth
                      , secure         = True
                      , host           = h
                      , path           = rqPath
                      , requestBody    = RequestBodyLBS (encode rqBody)
                      , requestHeaders = [("Accept", "application/json")]
                      }
         in case auth of
            Just (Token t) ->
                req { requestHeaders = ("Authorization", "Token token=" <> t)
                                     : requestHeaders req }
            Just (Basic u p) ->
                applyBasicAuth u p $ req

            _ -> req

response :: FromJSON a => Response L.ByteString -> IO (Either Error a)
response res = case statusCode (responseStatus res) of
    200 -> success
    201 -> success
    400 -> failure
    500 -> failure
    n   -> return . Left . Internal $
        "PagerDuty returned unhandled status code: " ++ show n
  where
    success = maybe (Left unknown) Right <$> parse
    failure = maybe (Left unknown) Left  <$> parse

    unknown = Internal
        "Unable to parse response into a PagerDuty API compatible type: <body>"

    parse :: FromJSON a => IO (Maybe a)
    parse = pure . decode . responseBody $ res
