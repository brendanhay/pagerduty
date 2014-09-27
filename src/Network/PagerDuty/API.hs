-- Module      : Network.PagerDuty.API
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Re-exports of the supported PagerDuty APIs
module Network.PagerDuty.API
    ( -- * Supported APIs
      module Network.PagerDuty.API.Integration
    , module Network.PagerDuty.API.MaintenanceWindows
    , module Network.PagerDuty.API.Services
    -- * Common Types
    , PagerDuty
    , SubDomain (..)
    , Token     (..)
    , BasicAuth (..)
    , Code      (..)
    , Error     (..)
    )
where

import Network.PagerDuty.API.Integration
import Network.PagerDuty.API.MaintenanceWindows
import Network.PagerDuty.API.Services
import Network.PagerDuty.Types
