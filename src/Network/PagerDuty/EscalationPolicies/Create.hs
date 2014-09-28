{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Module      : Network.PagerDuty.EscalationPolicies.Create
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates an existing escalation policy and rules.
--
-- @POST \/escalation_policies@
--
-- See: <http://developer.pagerduty.com/documentation/rest/escalation_policies/create>
module Network.PagerDuty.EscalationPolicies.Create
    ( createPolicy
    , cpName
    , cpRepeatEnabled
    , cpNumLoops
    , cpEscalationRules
    ) where

import Data.Aeson.Lens
import Data.Text                                  (Text)
import Network.HTTP.Types
import Network.PagerDuty.EscalationPolicies.Types
import Network.PagerDuty.TH
import Network.PagerDuty.Types

data CreatePolicy = CreatePolicy
    { _cpName            :: Text
    , _cpRepeatEnabled   :: Maybe Bool
    , _cpNumLoops        :: Maybe Int
    , _cpEscalationRules :: [Rule] -- ^ Should be List1
    } deriving (Eq, Show)

createPolicy :: PolicyId
             -> Text   -- ^ 'creName'
             -> [Rule] -- ^ 'creEscalationRules'
             -> Request CreatePolicy Token Policy
createPolicy i n rs = req PUT i (key "escalation_policy") $
    CreatePolicy
        { _cpName            = n
        , _cpRepeatEnabled   = Nothing
        , _cpNumLoops        = Nothing
        , _cpEscalationRules = rs
        }

-- | The name of the escalation policy.
makeLens "_cpName" ''CreatePolicy

-- | Whether or not to allow this policy to repeat its escalation
-- rules after the last rule is finished. Defaults to false.
makeLens "_cpRepeatEnabled" ''CreatePolicy

-- | The number of times to loop over the set of rules in this escalation policy.
makeLens "_cpNumLoops" ''CreatePolicy

-- | The escalation rules for this policy.
makeLens "_cpEscalationRules" ''CreatePolicy

deriveJSON ''CreatePolicy
