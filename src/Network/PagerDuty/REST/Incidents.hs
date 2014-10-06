{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Module      : Network.PagerDuty.REST.Incidents
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | PagerDuty receives updates (called events) from monitoring systems through
-- services (like Nagios, email or generic API calls). Upon receiving an event,
-- active services will create a new incident and begin escalating it as
-- defined by the service's escalation policy. Events can be de-duplicated into
-- existing incidents based on service de-duplication rules to prevent you from
-- being overwhelmed by event storms.
--
-- An incident can be open, acknowledged or resolved. Whenever an incident is
-- created, it is assigned to a user (according to the escalation process, as
-- described by escalation rules and schedules). The assigned user has a chance
-- to either acknowledge that he is working on it, or to resolve it.
--
-- Resolving an incident closes it, whereas acknowledging it halts the
-- escalation process. If the incident is not resolved by the service's
-- incident ack timeout it continues up the escalation chain.
--
-- When an incident is triggered or when it is escalated it creates alerts
-- (also known as notifications). Alerts are messages containing the details of
-- the incident, and can be sent through SMS, email and phone calls.
--
-- Incidents and Incident Counts from the last 30 days are returned by
-- default. To change this default date range, see the documentation for the
-- since, until or date_range parameters.
--
-- Note: that to create an incident you need to trigger it through the
-- "Network.PagerDuty.Integration" API.
module Network.PagerDuty.REST.Incidents
    (

    ) where

import Control.Lens
import Data.Monoid
import Data.Text                    (Text)
import Data.Time
import Network.HTTP.Types
import Network.PagerDuty.TH
import Network.PagerDuty.Types

incidents :: Path
incidents = "incidents"

-- | The PagerDuty incidents query API can be used to query current and
-- historical PagerDuty incidents over a date range, letting you build custom
-- dashboards or incident reports. The API allows for searching for incidents
-- with multiple filters or query parameters, various sorts, and also supports
-- the pagination of results.
--
-- @GET \/incidents@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/incidents/list>
listIncidents = undefined

-- | Get detailed information about an incident. Accepts either an incident id,
-- or an incident number.
--
-- @GET \/incidents\/\:id@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/incidents/show>
getIncident = undefined

-- | Use this query if you are simply looking for the count of incidents that
-- match a given query. This should be used if you don't need access to the
-- actual incident details.
--
-- @GET \/incidents\/count@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/incidents/count>
countIncidents = undefined

-- | Acknowledge, resolve, escalate or reassign one or more incidents.
--
-- @PUT \/incidents@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/incidents/update>
updateIncidents = undefined

-- | Resolve an incident.
--
-- @PUT \/incidents\/\:id\/resolve@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/incidents/resolve>
resolveIncident = undefined

-- | Acknowledge an incident.
--
-- @PUT \/incidents\/\:id\/acknowledge@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/incidents/acknowledge>
acknowledgeIncident = undefined

-- | Reassign an incident.
--
-- @PUT \/incidents\/\:id\/reassign@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/incidents/reassign>
reassignIncident = undefined
