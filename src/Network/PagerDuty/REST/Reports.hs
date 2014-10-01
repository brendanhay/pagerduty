{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Module      : Network.PagerDuty.REST.Reports
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Access high level reports about alerts and incidents. Useful for creating graphs.
module Network.PagerDuty.REST.Reports
    (

    ) where

reports :: Path
reports = "reports"

-- | Get high level statistics about the number of alerts (SMSes, phone calls and
-- emails) sent for the desired time period, summed daily, weekly or monthly.
--
-- @GET \/reports\/alerts_per_time@
--
-- See: <http://developer.pagerduty.com/documentation/rest/reports/alerts_per_time>
alertsPerTime = undefined

-- | Get high level statistics about the number of incidents created for the desired
-- time period, summed daily, weekly or monthly.
--
-- @GET \/reports\/incidents_per_time@
--
-- See: <http://developer.pagerduty.com/documentation/rest/reports/incidents_per_time>
incidentsPerTime = undefined
