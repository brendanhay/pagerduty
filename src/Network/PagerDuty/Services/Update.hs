{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Module      : Network.PagerDuty.Services.Update
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

--
-- @PUT services\/\:id@
--
-- See: <http://developer.pagerduty.com/documentation/rest/services/update>
module Network.PagerDuty.Services.Update
    (
    ) where

import Network.HTTP.Types
import Network.PagerDuty.Services.Types
import Network.PagerDuty.TH
import Network.PagerDuty.Types
