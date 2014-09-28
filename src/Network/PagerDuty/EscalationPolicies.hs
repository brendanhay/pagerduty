{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Module      : Network.PagerDuty.EscalationPolicies
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This API lets you access and manipulate escalation policies and rules.
--
-- See: <http://developer.pagerduty.com/documentation/rest/escalation_policies>
module Network.PagerDuty.EscalationPolicies
    (
    -- * List Policies
      module Network.PagerDuty.EscalationPolicies.List

    -- * Create Policy
    , module Network.PagerDuty.EscalationPolicies.Create

    -- * Get Policy
    , module Network.PagerDuty.EscalationPolicies.Get

    -- * Update Policy
    , module Network.PagerDuty.EscalationPolicies.Update

    -- * Delete Policy
    , module Network.PagerDuty.EscalationPolicies.Delete

    -- * Types
    , Target (..)
    , _TSchedule
    , _TUser

    , ScheduleTarget
    , stgtId
    , stgtName

    , UserTarget
    , utgtId
    , utgtName
    , utgtEmail
    , utgtTimeZone
    , utgtColor

    , Rule
    , ruleId
    , ruleEscalationDelayInMinutes
    , ruleTargets

    , Policy
    , policyId
    , policyName
    , policyNumLoops
    , policyRules
    , policyServices
    ) where

import Network.PagerDuty.EscalationPolicies.Create
import Network.PagerDuty.EscalationPolicies.List
import Network.PagerDuty.EscalationPolicies.Types
import Network.PagerDuty.EscalationPolicies.Update
import Network.PagerDuty.EscalationPolicies.Delete
import Network.PagerDuty.EscalationPolicies.Get
