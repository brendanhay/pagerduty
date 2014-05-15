{-# LANGUAGE OverloadedStrings #-}

module Network.PagerDuty.IO
    ( request
    )
where

import Control.Applicative
import Control.Monad.Reader
import Data.Aeson
import Data.Maybe
import Data.Monoid
import Network.HTTP.Client
import Network.HTTP.Types
import Network.PagerDuty.Types

import qualified Data.ByteString.Lazy as L


request :: (ToJSON a, FromJSON b) => Request -> a -> PagerDuty x (Either Error b)
request rq rqBody = liftIO . go =<< ask
  where
    go env = case env of
        AuthEnv h auth mgr -> httpLbs (rq' (Just h) (Just auth)) mgr >>= response
        Env            mgr -> httpLbs (rq' Nothing  Nothing)     mgr >>= response

    rq' h auth =
        let req = rq { secure         = True
                     , host           = fromMaybe (host rq) h
                     , port           = 443
                     , requestHeaders = [("Accept", "application/json")]
                     , requestBody    = RequestBodyLBS $ encode rqBody
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
            $ "Unable to parse response into a PagerDuty API compatible type: "
            ++ show (responseBody res)

    parse :: FromJSON a => IO (Maybe a)
    parse = pure . decode . responseBody $ res
