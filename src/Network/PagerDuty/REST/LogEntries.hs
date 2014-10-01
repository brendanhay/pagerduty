{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Module      : Network.PagerDuty.REST.LogEntries
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | PagerDuty keeps a log of all the events that happen to an incident. The
-- following APIs provide fine-grained access to this incident log entry data to
-- give you more insight into how your team or organization is handling your
-- incidents. Log entry data includes details about the event(s) that triggered
-- the incident, who was notified and when, how they were notified, and who
-- acknowledged or resolved it, amongst a few other things.
module Network.PagerDuty.REST.LogEntries
    (

    ) where

logs :: Path
logs = "log_entries"

-- | List all incident log entries across the entire account.
--
-- @GET \/log_entries@
--
-- See: <http://developer.pagerduty.com/documentation/rest/log_entries/list>
listLogs = undefined

-- | List all incident log entries that describe interactions with a specific user.
--
-- @GET \/users\/\:user_id\/log_entries@
--
-- See: <http://developer.pagerduty.com/documentation/rest/log_entries/user_log_entries>
listUserLogs = undefined

-- | List all incident log entries for a specific incident.
--
-- @GET \/incidents\/\:incident_id\/log_entries@
--
-- See: <http://developer.pagerduty.com/documentation/rest/log_entries/incident_log_entries>
listIncidentLogs = undefined

-- | Get details for a specific incident log entry. This method provides additional
-- information you can use to get at raw event data.
--
-- @GET \/log_entries\/\:id@
--
-- See: <http://developer.pagerduty.com/documentation/rest/log_entries/show>
getLog = undefined
