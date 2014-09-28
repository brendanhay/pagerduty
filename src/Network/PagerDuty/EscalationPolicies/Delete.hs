{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Module      : Network.PagerDuty.EscalationPolicies.Delete
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.PagerDuty.EscalationPolicies.Delete
    ( deletePolicy
    ) where

import Network.HTTP.Types
import Network.PagerDuty.EscalationPolicies.Types
import Network.PagerDuty.TH
import Network.PagerDuty.Types

data DeletePolicy = DeletePolicy

deriveJSON ''DeletePolicy

-- FIXME: Expected response 204 no content
-- | Deletes an existing escalation policy and rules. The escalation policy
-- must not be in use by any services.
--
-- @DELETE \/escalation_policies\/\:id@
--
-- See: <http://developer.pagerduty.com/documentation/rest/escalation_policies/delete>
deletePolicy :: PolicyId -> Request DeletePolicy Token Empty
deletePolicy i = req DELETE i unwrap DeletePolicy
