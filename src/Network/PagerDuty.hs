{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Network.PagerDuty
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.PagerDuty
    ( withAuth
    , noAuth
    , module Network.PagerDuty.API
    )
where

import Control.Monad.Reader
import Data.Monoid
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.PagerDuty.API
import Network.PagerDuty.Types


withAuth :: MonadIO m => SubDomain -> Auth -> PagerDuty Authenticated b -> m b
withAuth sd auth pd = run pd (AuthEnv (addr sd) auth)

noAuth :: MonadIO m => PagerDuty UnAuthenticated b -> m b
noAuth pd = run pd Env

run :: MonadIO m => PagerDuty a b -> (Manager -> Env a) -> m b
run pd env = liftIO . withManager tlsManagerSettings $ runReaderT pd . env

addr :: SubDomain -> Host
addr = (<> ".pagerduty.com") . subDomain
