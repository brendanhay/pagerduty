{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Module      : Network.PagerDuty.Alerts
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | When an incident is triggered or when it is escalated it creates an alert
-- (also known as a notification). Alerts are messages containing the details
-- of the incident, and can be sent through SMS, email, phone calls,
-- and push notifications.
--
-- This API allows you to access read-only data regarding what alerts have been
-- sent to your users.
--
-- See: <http://developer.pagerduty.com/documentation/rest/alerts>
module Network.PagerDuty.Alerts
    (
    -- * List Alerts
      module Network.PagerDuty.Alerts.List

    -- * Types
    , AlertType (..)

    , Alert
    , alertId
    , alertType
    , alertStartedAt
    , alertUser
    , alertAddress
    ) where

import Network.PagerDuty.Alerts.List
import Network.PagerDuty.Alerts.Types
