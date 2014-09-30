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
import           Data.Aeson               hiding (Error)
import qualified Data.ByteString.Lazy     as LBS
import           Data.Conduit
import           Data.Default
import           Data.Monoid
import           Network.HTTP.Client      (Manager, applyBasicAuth)
import qualified Network.HTTP.Client.Lens as Lens
import           Network.HTTP.Types
import           Network.PagerDuty.HTTP
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
    & Lens.host        .~ h
    & Lens.path        .~ renderPath (rq ^. path)
    & Lens.queryString .~ renderQuery False (rq ^. query)
  where
    raw = case a of
        AuthBasic u p -> applyBasicAuth u p def
        AuthToken t   -> def & Lens.requestHeaders <>~
            [("Authorization", "Token token=" <> t)]
