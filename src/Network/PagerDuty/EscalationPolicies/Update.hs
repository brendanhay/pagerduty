{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Module      : Network.PagerDuty.EscalationPolicies.Update
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates an existing escalation policy and rules.
--
-- @PUT \/escalation_policies\/\:id@
--
-- See: <http://developer.pagerduty.com/documentation/rest/escalation_policies/update>
module Network.PagerDuty.EscalationPolicies.Update
    ( updatePolicy
    , upName
    , upRepeatEnabled
    , upNumLoops
    , upEscalationRules
    ) where

import Data.Aeson.Lens
import Data.Text                                  (Text)
import Network.HTTP.Types
import Network.PagerDuty.EscalationPolicies.Types
import Network.PagerDuty.TH
import Network.PagerDuty.Types

data UpdatePolicy = UpdatePolicy
    { _upName            :: Maybe Text
    , _upRepeatEnabled   :: Maybe Bool
    , _upNumLoops        :: Maybe Int
    , _upEscalationRules :: [Rule]
    } deriving (Eq, Show)

updatePolicy :: PolicyId -> Request UpdatePolicy Token Policy
updatePolicy i = req PUT i (key "escalation_policy") $
    UpdatePolicy
        { _upName            = Nothing
        , _upRepeatEnabled   = Nothing
        , _upNumLoops        = Nothing
        , _upEscalationRules = []
        }

-- | The name of the escalation policy.
makeLens "_upName" ''UpdatePolicy

-- | Whether or not to allow this policy to repeat its escalation
-- rules after the last rule is finished.
makeLens "_upRepeatEnabled" ''UpdatePolicy

-- | The number of times to loop over the set of rules in this escalation policy.
makeLens "_upNumLoops" ''UpdatePolicy

-- | The escalation rules for this policy.
makeLens "_upEscalationRules" ''UpdatePolicy

deriveJSON ''UpdatePolicy
