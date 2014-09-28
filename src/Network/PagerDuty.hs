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

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import           Control.Monad.Trans.Resource
import           Data.Aeson                   hiding (Error)
import qualified Data.ByteString.Lazy         as LBS
import           Data.Conduit
import qualified Network.HTTP.Client          as Client
import qualified Network.HTTP.Conduit         as Client
import           Network.HTTP.Conduit         hiding (Request)
import           Network.HTTP.Types
import           Network.PagerDuty.Types

-- data Env (a :: Auth) = Env
--     { _envDomain  :: Domain
--     , _envAuth    :: Cred a
--     , _envManager :: Manager
--     }

send :: (MonadIO m, FromJSON r)
     => Cred a
     -> Domain
     -> Manager
     -> Request s a r
     -> m (Either Error r)
send c d m x = httpLbs (request x) m >>= response x

paginate :: (MonadResource m, FromJSON r, Paginate r)
         => Cred a
         -> Domain
         -> Manager
         -> Request s a r
         -> Source m (Either Error r)
paginate c d m = go
  where
    go x = do
        y <- lift (send c d m x)
        yield y
        either (const (return ()))
               (maybe (return ()) go . next x)
               y

request :: Request s a r -> Client.Request
request (Request x rq) = rq

response :: (MonadIO m, FromJSON r)
         => Request s a r
         -> Client.Response LBS.ByteString
         -> m (Either Error r)
response _ x = case statusCode (responseStatus x) of
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

    body = responseBody x


-- withToken :: MonadIO m
--           => Token
--           -> SubDomain
--           -> PagerDuty (Authenticated Token) b
--           -> m b
-- withToken tok sd pd = run pd (TokenEnv (addr sd) tok)

-- withBasicAuth :: MonadIO m
--               => BasicAuth
--               -> SubDomain
--               -> PagerDuty (Authenticated BasicAuth) b
--               -> m b
-- withBasicAuth bas sd pd = run pd (BasicEnv (addr sd) bas)

-- unAuthenticated :: MonadIO m => PagerDuty UnAuthenticated b -> m b
-- unAuthenticated pd = run pd Env

-- run :: MonadIO m => PagerDuty a b -> (Manager -> Env a) -> m b
-- run pd env = liftIO . withManager tlsManagerSettings $ runReaderT pd . env

-- addr :: SubDomain -> Host
-- addr = (<> ".pagerduty.com") . subDomain
