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

module Network.PagerDuty
    (
    -- * Run PagerDuty actions
      send
    , paginate

    , module Network.PagerDuty.Types
    ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import           Data.Aeson                 hiding (Error)
import qualified Data.ByteString.Lazy       as LBS
import           Data.Conduit
import           Data.Default
import           Data.Monoid
import           Network.HTTP.Client        (Manager, httpLbs)
import qualified Network.HTTP.Client        as Client
import qualified Network.HTTP.Client.Lens   as Lens
import           Network.HTTP.Types
import           Network.PagerDuty.Types

-- data Env (s :: Security) = Env
--     { _envDomain  :: Domain
--     , _envAuth    :: Auth s
--     , _envManager :: Manager
--     }

send :: (MonadIO m, FromJSON r)
     => Auth s
     -> SubDomain
     -> Manager
     -> Request a s r
     -> m (Either Error r)
send a s m = liftM (fmap fst) . http a s m

paginate :: (MonadIO m, Paginate a, FromJSON r)
         => Auth s
         -> SubDomain
         -> Manager
         -> Request a s r
         -> Source m (Either Error r)
paginate a d m = go
  where
    go rq = do
        rs <- lift (http a d m rq)
        yield  (fst <$> rs)
        either (const (return ()))
               (maybe (return ()) go . next rq . snd)
               rs

http :: (MonadIO m, FromJSON r)
         => Auth s
         -> SubDomain
         -> Manager
         -> Request a s r
         -> m (Either Error (r, Maybe Pager))
http a (SubDomain h) m rq = liftIO (httpLbs raw m) >>= response rq
  where
    raw = authorise
        & Lens.secure         .~ True
        & Lens.port           .~ 443
        & Lens.path           .~ renderPath (rq^.path)
        & Lens.queryString    .~ renderQuery False (rq^.query)
        & Lens.requestHeaders <>~ headers
        & Lens.requestBody    .~ Client.RequestBodyLBS (encode rq)

    authorise = case a of
        AuthBasic u p -> Client.applyBasicAuth u p def & Lens.host .~ h
        AuthToken t   -> def & Lens.host .~ h & Lens.requestHeaders <>~ [token t]
        _             -> def

    token t = ("Authorization", "Token token=" <> t)

    headers =
        [ ("Content-Type", "application/json")
        , ("Accept",       "application/json")
        ]

response :: (MonadIO m, FromJSON r)
         => Request a s r
         -> Client.Response LBS.ByteString
         -> m (Either Error (r, Maybe Pager))
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
