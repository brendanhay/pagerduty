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

import Control.Lens
import Data.Aeson.Lens
import Data.Text                                  (Text)
import Network.HTTP.Types
import Network.PagerDuty.EscalationPolicies.Types
import Network.PagerDuty.TH
import Network.PagerDuty.Types

policies :: Setter (Request a s r) (Request a s r) Path [Path]
policies = base "policies"

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
listPolicies =
    mk ListPolicies
        { _lpQuery' = Nothing
        } & policies .~ []
          & unwrap   .~ key "escalation_policies"

-- | Filters the result, showing only the escalation policies
-- whose names match the query.
lpQuery :: Lens' (Request ListPolicies s r) (Maybe Text)
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
createPolicy i n rs =
    mk CreatePolicy
        { _cpName'            = n
        , _cpRepeatEnabled'   = Nothing
        , _cpNumLoops'        = Nothing
        , _cpEscalationRules' = rs
        } & policies .~ [P i]
          & meth     .~ PUT
          & unwrap   .~ key "escalation_policy"

-- | The name of the escalation policy.
cpName :: Lens' (Request CreatePolicy s r) Text
cpName = upd.cpName'

-- | Whether or not to allow this policy to repeat its escalation
-- rules after the last rule is finished.
--
-- Defaults to false.
cpRepeatEnabled :: Lens' (Request CreatePolicy s r) (Maybe Bool)
cpRepeatEnabled = upd.cpRepeatEnabled'

-- | The number of times to loop over the set of rules in this escalation policy.
cpNumLoops :: Lens' (Request CreatePolicy s r) (Maybe Int)
cpNumLoops = upd.cpNumLoops'

-- | The escalation rules for this policy.
cpEscalationRules :: Lens' (Request CreatePolicy s r) [Rule]
cpEscalationRules = upd.cpEscalationRules'

-- | Get information about an existing escalation policy and its rules.
--
-- @GET \/escalation_policies\/\:id@
--
-- See: <http://developer.pagerduty.com/documentation/rest/escalation_policies/show>
getPolicy :: PolicyId -> Request Empty Token Policy
getPolicy i = mk Empty & policies .~ [P i] & unwrap .~ key "escalation_policy"

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
updatePolicy i =
    mk UpdatePolicy
        { _upName'            = Nothing
        , _upRepeatEnabled'   = Nothing
        , _upNumLoops'        = Nothing
        , _upEscalationRules' = []
        } & policies .~ [P i]
          & meth     .~ PUT
          & unwrap   .~ key "escalation_policy"

-- | The name of the escalation policy.
upName :: Lens' (Request UpdatePolicy s r) (Maybe Text)
upName = upd.upName'

-- | Whether or not to allow this policy to repeat its escalation
-- rules after the last rule is finished.
upRepeatEnabled :: Lens' (Request UpdatePolicy s r) (Maybe Bool)
upRepeatEnabled = upd.upRepeatEnabled'

-- | The number of times to loop over the set of rules in this escalation policy.
upNumLoops :: Lens' (Request UpdatePolicy s r) (Maybe Int)
upNumLoops = upd.upNumLoops'

-- | The escalation rules for this policy.
upEscalationRules :: Lens' (Request UpdatePolicy s r) [Rule]
upEscalationRules = upd.upEscalationRules'

-- | Deletes an existing escalation policy and rules. The escalation policy
-- must not be in use by any services.
--
-- @DELETE \/escalation_policies\/\:id@
--
-- See: <http://developer.pagerduty.com/documentation/rest/escalation_policies/delete>
deletePolicy :: PolicyId -> Request Empty Token Empty
deletePolicy i = mk Empty & policies .~ [P i] & meth .~ DELETE
