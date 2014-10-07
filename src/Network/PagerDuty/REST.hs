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
    ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import           Data.Aeson              (FromJSON)
import           Data.Conduit
import           Data.Default
import           Data.Monoid
import           Network.HTTP.Client     (Manager)
import qualified Network.HTTP.Client     as Client
import           Network.HTTP.Types
import           Network.PagerDuty.IO
import           Network.PagerDuty.Types

-- | Make the type names consistent, for example:
-- Suffix with *Details or *Info for results
-- No suffix for requests if necessary

send :: (MonadIO m, FromJSON b)
     => Auth s
     -> SubDomain
     -> Manager
     -> Request a s b
     -> m (Either Error b)
send a s m = liftM (fmap fst) . http a s m

paginate :: (MonadIO m, Paginate a, FromJSON b)
         => Auth s
         -> SubDomain
         -> Manager
         -> Request a s b
         -> Source m (Either Error b)
paginate a d m = go
  where
    go rq = do
        rs <- lift (http a d m rq)
        yield  (fst <$> rs)
        either (const (return ()))
               (maybe (return ()) go . next rq . snd)
               rs

http :: (MonadIO m, FromJSON b)
     => Auth s
     -> SubDomain
     -> Manager
     -> Request a s b
     -> m (Either Error (b, Maybe Pager))
http a (SubDomain h) m rq = request m rq $ raw
    { Client.host        = h
    , Client.path        = renderPath (rq ^. path)
    , Client.queryString = renderQuery False (rq ^. query)
    }
  where
    raw = case a of
        AuthBasic u p -> Client.applyBasicAuth u p def
        AuthToken t   -> def
            { Client.requestHeaders = [("Authorization", "Token token=" <> t)]
            }
