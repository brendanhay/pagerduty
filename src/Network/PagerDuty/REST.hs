{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.PagerDuty.REST
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.PagerDuty.REST
    (

    -- * Sending requests
      send
    , paginate

    -- * Configuration
    -- ** Environment
    , Env
    , envDomain
    , envAuth
    , envManager
    , envLogger

    -- ** Requests
    , sendWith
    , paginateWith
    ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import           Data.Aeson              (FromJSON)
import           Data.Conduit
import           Data.Default.Class
import           Data.Monoid
import           Network.HTTP.Client     (Manager)
import qualified Network.HTTP.Client     as Client
import           Network.HTTP.Types
import           Network.PagerDuty.Internal.IO
import           Network.PagerDuty.Internal.Types

-- FIXME: verify correct actions are all paginated
-- FIXME: Ensure RequesterId parameter is always most significant param
-- FIXME: add smart constructors for all types, for testing purposes

send :: (MonadIO m, FromJSON b)
     => SubDomain
     -> Auth s
     -> Manager
     -> Request a s b
     -> m (Either Error b)
send d a m = sendWith (prod d a m)

-- | /See:/ 'sendWith'
sendWith :: (MonadIO m, FromJSON b)
         => Env s
         -> Request a s b
         -> m (Either Error b)
sendWith e = liftM (fmap fst) . http e

paginate :: (MonadIO m, Paginate a, FromJSON b)
         => SubDomain
         -> Auth s
         -> Manager
         -> Request a s b
         -> Source m (Either Error b)
paginate d a m = paginateWith (prod d a m)

-- | /See:/ 'paginateWith'
paginateWith :: (MonadIO m, Paginate a, FromJSON b)
             => Env s
             -> Request a s b
             -> Source m (Either Error b)
paginateWith e = go
  where
    go rq = do
        rs <- lift (http e rq)
        yield  (fst <$> rs)
        either (const (return ()))
               (maybe (return ()) go . next rq . snd)
               rs

http :: (MonadIO m, FromJSON b)
     => Env s
     -> Request a s b
     -> m (Either Error (b, Maybe Pager))
http e rq = request (e ^. envManager) (e ^. envLogger) rq $ raw
    { Client.host        = domain (e ^. envDomain)
    , Client.path        = renderPath (rq ^. path)
    , Client.queryString = renderQuery False (rq ^. query)
    }
  where
   raw = case e ^. envAuth of
        AuthBasic u p -> Client.applyBasicAuth u p def
        AuthToken t   -> def
            { Client.requestHeaders = [("Authorization", "Token token=" <> t)]
            }
