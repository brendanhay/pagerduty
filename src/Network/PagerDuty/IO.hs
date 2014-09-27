{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.PagerDuty.IO
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.PagerDuty.IO
    ( request
    , defaultRequest
    , Request (..)

    -- * Re-exported
    , module HTTP
    ) where

import           Control.Applicative
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.ByteString.Lazy    as LBS
import           Data.Default
import           Data.Maybe
import           Data.Monoid
import           Network.HTTP.Client
import           Network.HTTP.Types      as HTTP
import           Network.PagerDuty.Types

defaultRequest :: Request
defaultRequest = def

request :: (ToJSON a, FromJSON b) => Request -> a -> PagerDuty x (Either Error b)
request rq rqBody = liftIO . go =<< ask
  where
    go env = httpLbs req mgr >>= response
      where
        (req,mgr) = case env of
            TokenEnv h tok m -> (authToken tok . rq' $ Just h, m)
            BasicEnv h bas m -> (authBasic bas . rq' $ Just h, m)
            Env            m -> (rq' Nothing, m)

    rq' h = rq { secure         = True
               , host           = fromMaybe (host rq) h
               , port           = 443
               , requestHeaders = [ ("Accept", "application/json")
                                  , ("Content-Type", "application/json")
                                  ]
               , requestBody    = RequestBodyLBS $ encode rqBody
               }

    authToken (Token t) req =
        req { requestHeaders = ("Authorization", "Token token=" <> t)
                             : requestHeaders req }

    authBasic (BasicAuth u p) = applyBasicAuth u p

response :: FromJSON a => Response LBS.ByteString -> IO (Either Error a)
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
