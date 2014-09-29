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
      listPolicies
    , lpQuery

    -- * Create Policy
    , createPolicy
    , cpName
    , cpRepeatEnabled
    , cpNumLoops
    , cpEscalationRules

    -- * Get Policy
    , getPolicy

    -- * Update Policy
    , updatePolicy
    , upName
    , upRepeatEnabled
    , upNumLoops
    , upEscalationRules

    -- * Delete Policy
    , deletePolicy

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

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Char8                      as BS
import           Data.ByteString.Conversion
import           Data.Text                                  (Text)
import           Network.HTTP.Types
import           Network.PagerDuty.EscalationPolicies.Types
import           Network.PagerDuty.JSON
import           Network.PagerDuty.Types

newtype ListPolicies = ListPolicies
    { _lpQuery' :: Maybe Text
    } deriving (Eq, Show)

instance Paginate ListPolicies

deriveJSON ''ListPolicies
makeLenses ''ListPolicies

-- | List all the existing escalation policies.
--
-- @GET \/escalation_policies@
--
-- See: <http://developer.pagerduty.com/documentation/rest/escalation_policies/list>
listPolicies :: Request ListPolicies Token [Policy]
listPolicies = mk (unwrap .~ key "escalation_policies")
    ListPolicies
        { _lpQuery' = Nothing
        }

-- | Filters the result, showing only the escalation policies
-- whose names match the query.
lpQuery :: Lens' (Request ListPolicies s r) a
lpQuery = upd.lpQuery'

data CreatePolicy = CreatePolicy
    { _cpName'            :: Text
    , _cpRepeatEnabled'   :: Maybe Bool
    , _cpNumLoops'        :: Maybe Int
    , _cpEscalationRules' :: [Rule] -- ^ Should be List1
    } deriving (Eq, Show)

deriveJSON ''CreatePolicy
makeLenses ''CreatePolicy

-- | Creates an existing escalation policy and rules.
--
-- @POST \/escalation_policies@
--
-- See: <http://developer.pagerduty.com/documentation/rest/escalation_policies/create>
createPolicy :: PolicyId
             -> Text   -- ^ 'creName'
             -> [Rule] -- ^ 'creEscalationRules'
             -> Request CreatePolicy Token Policy
createPolicy i n rs = mk
    (method .~ PUT & path .~ i & unwrap .~ key "escalation_policy")
    CreatePolicy
        { _cpName'            = n
        , _cpRepeatEnabled'   = Nothing
        , _cpNumLoops'        = Nothing
        , _cpEscalationRules' = rs
        }

-- | The name of the escalation policy.
cpName :: Lens' (Request CreatePolicy s r) a
cpName = upd.cpName'

-- | Whether or not to allow this policy to repeat its escalation
-- rules after the last rule is finished. Defaults to false.
cpRepeatEnabled :: Lens' (Request CreatePolicy s r) a
cpRepeatEnabled = upd.cpRepeatEnabled'

-- | The number of times to loop over the set of rules in this escalation policy.
cpNumLoops :: Lens' (Request CreatePolicy s r) a
cpNumLoops = upd.cpNumLoops'

-- | The escalation rules for this policy.
cpEscalationRules :: Lens' (Request CreatePolicy s r) a
cpEscalationRules = upd.cpEscalationRules'

-- | Get information about an existing escalation policy and its rules.
--
-- @GET \/escalation_policies\/\:id@
--
-- See: <http://developer.pagerduty.com/documentation/rest/escalation_policies/show>
getPolicy :: PolicyId -> Request Empty Token Policy
getPolicy i = mk (path .~ i & unwrap .~ key "escalation_policy") Empty

data UpdatePolicy = UpdatePolicy
    { _upName'            :: Maybe Text
    , _upRepeatEnabled'   :: Maybe Bool
    , _upNumLoops'        :: Maybe Int
    , _upEscalationRules' :: [Rule]
    } deriving (Eq, Show)

deriveJSON ''UpdatePolicy
makeLenses ''UpdatePolicy

-- | Updates an existing escalation policy and rules.
--
-- @PUT \/escalation_policies\/\:id@
--
-- See: <http://developer.pagerduty.com/documentation/rest/escalation_policies/update>
updatePolicy :: PolicyId -> Request UpdatePolicy Token Policy
updatePolicy i = mk
    (method .~ PUT & path .~ i & unwrap .~ key "escalation_policy")
    UpdatePolicy
        { _upName'            = Nothing
        , _upRepeatEnabled'   = Nothing
        , _upNumLoops'        = Nothing
        , _upEscalationRules' = []
        }

-- | The name of the escalation policy.
upName :: Lens' (Request UpdatePolicy s r) a
upName = upd.upName'

-- | Whether or not to allow this policy to repeat its escalation
-- rules after the last rule is finished.
upRepeatEnabled :: Lens' (Request UpdatePolicy s r) a
upRepeatEnabled = upd.upRepeatEnabled'

-- | The number of times to loop over the set of rules in this escalation policy.
upNumLoops :: Lens' (Request UpdatePolicy s r) a
upNumLoops = upd.upNumLoops'

-- | The escalation rules for this policy.
upEscalationRules :: Lens' (Request UpdatePolicy s r) a
upEscalationRules = upd.upEscalationRules'

-- | Deletes an existing escalation policy and rules. The escalation policy
-- must not be in use by any services.
--
-- @DELETE \/escalation_policies\/\:id@
--
-- See: <http://developer.pagerduty.com/documentation/rest/escalation_policies/delete>
deletePolicy :: PolicyId -> Request Empty Token Empty
deletePolicy i = req DELETE i unwrap Empty
