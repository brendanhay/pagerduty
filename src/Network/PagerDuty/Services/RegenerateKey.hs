{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Module      : Network.PagerDuty.Services.RegenerateKey
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Regenerate a new service key for an existing service.
--
-- _Warning! The service's previous key will be invalidated, and existing
-- monitoring integrations will need to be modified to use the new key!_
--
-- @POST services\/\:id\/regenerate_key@
--
-- See: <http://developer.pagerduty.com/documentation/rest/services/regenerate_key>
module Network.PagerDuty.Services.RegenerateKey
    ( regenerateKey
    ) where

import Data.ByteString.Conversion
import Data.Monoid
import Network.HTTP.Types
import Network.PagerDuty.Services.Types
import Network.PagerDuty.TH
import Network.PagerDuty.Types

data RegenerateKey = RegenerateKey

deriveJSON ''RegenerateKey

regenerateKey :: ServiceId -> Request RegenerateKey Token Service
regenerateKey i =
    req POST (toByteString i <> "/regenerate_key") unwrap RegenerateKey
