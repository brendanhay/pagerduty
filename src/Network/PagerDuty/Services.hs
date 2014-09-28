{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Module      : Network.PagerDuty.Services
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This API lets you access and manipulate the services across your account.
--
-- A service is an endpoint (like Nagios, email, or an API call) that
-- generates events, which Pagerduty normalizes and dedupes, creating incidents.
--
-- When a service is shown inlined in other resources, a deleted service will
-- have its 'svcHtmlUrl attribute set to 'Nothing'.
--
-- See: <http://developer.pagerduty.com/documentation/rest/services>
module Network.PagerDuty.Services
    (
    -- * List Services
      module Network.PagerDuty.Services.List

    -- * Create Service
    , module Network.PagerDuty.Services.Create

    -- * Get Service

    -- * Update Service

    -- * Delete Service

    -- * Disable Service

    -- * Enable Service

    -- * Regenerate Key

    -- * Types
    , EmailFilterMode       (..)
    , EmailIncidentCreation (..)
    , MatchMode             (..)
    , ServiceStatus         (..)
    , ServiceType           (..)
    , SeverityFilter        (..)

    , IncidentCounts
    , cntTriggered
    , cntAcknowledged
    , cntResolved
    , cntTotal

    , EmailFilters          (..)
    , efsId
    , efsSubjectMode
    , efsSubjectRegex
    , efsBodyMode
    , efsBodyRegex
    , efsFromEmailMode
    , efsFromEmailRegex

    , PolicyInfo
    , pinfoId
    , pinfoName

    , Service
    , svcId
    , svcName
    , svcDescription
    , svcServiceUrl
    , svcServiceKey
    , svcAutoResolveTimeout
    , svcAcknowledgementTimeout
    , svcCreatedAt
    , svcStatus
    , svcLastIncidentTimestamp
    , svcEmailIncidentCreation
    , svcIncidentCounts
    , svcEmailFilterMode
    , svcType
    , svcEscalationPolicy
    , svcEmailFilters
    , svcSeverityFilter
    ) where

import Network.PagerDuty.Services.Create
import Network.PagerDuty.Services.List
import Network.PagerDuty.Services.Types
