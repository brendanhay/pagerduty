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
    ( withToken
    , withBasicAuth
    , unAuthenticated
    , module Network.PagerDuty.API
    )
where

import Control.Monad.Reader
import Data.Monoid
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.PagerDuty.API
import Network.PagerDuty.Types


withToken :: MonadIO m
          => Token
          -> SubDomain
          -> PagerDuty (Authenticated Token) b
          -> m b
withToken tok sd pd = run pd (TokenEnv (addr sd) tok)

withBasicAuth :: MonadIO m
              => BasicAuth
              -> SubDomain
              -> PagerDuty (Authenticated BasicAuth) b
              -> m b
withBasicAuth bas sd pd = run pd (BasicEnv (addr sd) bas)

unAuthenticated :: MonadIO m => PagerDuty UnAuthenticated b -> m b
unAuthenticated pd = run pd Env

run :: MonadIO m => PagerDuty a b -> (Manager -> Env a) -> m b
run pd env = liftIO . withManager tlsManagerSettings $ runReaderT pd . env

addr :: SubDomain -> Host
addr = (<> ".pagerduty.com") . subDomain
