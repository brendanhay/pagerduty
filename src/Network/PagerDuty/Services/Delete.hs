{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Module      : Network.PagerDuty.Services.Delete
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Delete an existing service. Once the service is deleted, it will not be
-- accessible from the web UI and new incidents won't be able to be created
-- for this service.
--
-- @DELETE \/services/:id@
--
-- See: <http://developer.pagerduty.com/documentation/rest/services/delete>
module Network.PagerDuty.Services.Delete
    ( deleteService
    ) where

import Network.HTTP.Types
import Network.PagerDuty.Services.Types
import Network.PagerDuty.TH
import Network.PagerDuty.Types

data DeleteService = DeleteService

deriveJSON ''DeleteService

deleteService :: ServiceId -> Request DeleteService Token Empty
deleteService i = req DELETE i unwrap DeleteService
