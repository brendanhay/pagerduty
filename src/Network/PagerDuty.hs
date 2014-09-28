{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.PagerDuty
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.PagerDuty where
    -- (
    -- -- * Run PagerDuty actions
    --   withToken
    -- , withBasicAuth
    -- , unAuthenticated

    -- -- * Re-exported
    -- , module Network.PagerDuty.API
    -- ) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import           Data.Aeson               hiding (Error)
import qualified Data.ByteString.Lazy     as LBS
import           Data.Conduit
import           Data.Monoid
import           Network.HTTP.Client      (Manager, httpLbs)
import qualified Network.HTTP.Client      as Client
import           Network.HTTP.Client.Lens
import           Network.HTTP.Types
import           Network.PagerDuty.Types

-- data Env (s :: Security) = Env
--     { _envDomain  :: Domain
--     , _envAuth    :: Auth s
--     , _envManager :: Manager
--     }

send :: (MonadIO m, FromJSON b)
     => Auth s
     -> SubDomain
     -> Manager
     -> Request a s b
     -> m (Either Error b)
send a s m x = liftIO (httpLbs (request a s x) m) >>= response x

paginate :: (MonadIO m, Paginate a, FromJSON b)
         => Auth s
         -> SubDomain
         -> Manager
         -> Request a s b
         -> Source m (Either Error b)
paginate a d m = go
  where
    go x = do
        y <- lift (send a d m x)
        yield y
        either (const (return ()))
               (maybe (return ()) go . next x)
               y

request :: Auth s -> SubDomain -> Request a s b -> Client.Request
request a (SubDomain h) (Request x rq) = auth
    & secure         .~ True
    & port           .~ 443
    & requestHeaders <>~ headers
    & requestBody    .~ Client.RequestBodyLBS (encode x)
  where
    auth = case a of
        AuthBasic u p -> Client.applyBasicAuth u p rq & host .~ h
        AuthToken t   -> rq & host .~ h & requestHeaders <>~ [token t]
        _             -> rq

    headers =
        [ ("Accept",       "application/json")
        , ("Content-Type", "application/json")
        ]

    token t = ("Authorization", "Token token=" <> t)

response :: (MonadIO m, FromJSON b)
         => Request a s b
         -> Client.Response LBS.ByteString
         -> m (Either Error b)
response _ x = case statusCode (Client.responseStatus x) of
    200 -> success
    201 -> success
    400 -> failure
    500 -> failure
    n   -> return . Left $ unhandled n
  where
    success = maybe (Left unknown) Right `liftM` parse
    failure = maybe (Left unknown) Left  `liftM` parse

    unhandled n = Internal
        $ "PagerDuty returned unhandled status code: " ++ show n

    unknown = Internal
        $ "Unable to parse response into a PagerDuty API compatible type: "
        ++ show body

    parse :: (Monad m, FromJSON a) => m (Maybe a)
    parse = return (decode body)

    body = Client.responseBody x
