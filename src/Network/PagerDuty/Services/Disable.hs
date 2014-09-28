{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Module      : Network.PagerDuty.Services.Disable
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Disable a service. Once a service is disabled, it will not be able to
-- create incidents until it is enabled again.
--
-- @PUT services\/\:id\/disable@
--
-- See: <http://developer.pagerduty.com/documentation/rest/services/disable>
module Network.PagerDuty.Services.Disable
    ( disableService
    ) where

import Data.ByteString.Conversion
import Data.Monoid
import Network.HTTP.Types
import Network.PagerDuty.Services.Types
import Network.PagerDuty.TH
import Network.PagerDuty.Types

data DisableService = DisableService

deriveJSON ''DisableService

disableService :: ServiceId -> Request DisableService Token Empty
disableService i = req PUT (toByteString i <> "/disable") unwrap DisableService
