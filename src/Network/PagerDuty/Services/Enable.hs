{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Module      : Network.PagerDuty.Services.Enable
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Enable a previously disabled service.
--
-- @PUT services\/\:id\/enable@
--
-- See: <http://developer.pagerduty.com/documentation/rest/services/enable>
module Network.PagerDuty.Services.Enable
    ( enableService
    ) where

import Data.ByteString.Conversion
import Data.Monoid
import Network.HTTP.Types
import Network.PagerDuty.Services.Types
import Network.PagerDuty.TH
import Network.PagerDuty.Types

data EnableService = EnableService

deriveJSON ''EnableService

enableService :: ServiceId -> Request EnableService Token Empty
enableService i = req PUT (toByteString i <> "/enable") unwrap EnableService
