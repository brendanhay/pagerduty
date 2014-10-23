{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- Module      : Network.PagerDuty.REST.EscalationPolicies
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
-- /See:/ <http://developer.pagerduty.com/documentation/rest/escalation_policies>
module Network.PagerDuty.REST.EscalationPolicies
    (
    -- * List Policies
      ListPolicies
    , listPolicies
    , lpQuery

    -- * List On Call Policies
    , listOnCallPolicies

    -- * Create Policy
    , CreatePolicy
    , createPolicy
    , cpName
    , cpRepeatEnabled
    , cpNumLoops
    , cpEscalationRules

    -- * Get Policy
    , getPolicy

    -- * Update Policy
    , UpdatePolicy
    , updatePolicy
    , upName
    , upRepeatEnabled
    , upNumLoops
    , upEscalationRules

    -- * Delete Policy
    , deletePolicy

    -- * Types
    , Policy
    , pId
    , pName
    , pNumLoops
    , pRules
    , pServices
    ) where

import Control.Lens
import Data.Aeson.Lens
import Data.Text                       (Text)
import Network.HTTP.Types
import Network.PagerDuty.REST.Services (Service)
import Network.PagerDuty.TH
import Network.PagerDuty.Types

import Network.PagerDuty.REST.EscalationPolicies.EscalationRules

default (Path)

policies :: Path
policies = "escalation_policies"

data Policy = Policy
    { _pId       :: EscalationPolicyId
    , _pName     :: Text
    , _pNumLoops :: !Int
    , _pRules    :: [Rule]
    , _pServices :: [Service]
    } deriving (Eq, Show)

deriveJSON ''Policy

-- | The ID of the escalation policy.
makeLens "_pId" ''Policy

-- | The name of the escalation policy.
makeLens "_pName" ''Policy

-- | The number of times the escalation policy will repeat after
-- reaching the end of its escalation.
makeLens "_pNumLoops" ''Policy

-- | A list of the policy's escalation rules in order of escalation.
makeLens "_pRules" ''Policy

-- | A list of services using this escalation policy.
makeLens "_pServices" ''Policy

newtype ListPolicies = ListPolicies
    { _lpQuery' :: Maybe Text
    } deriving (Eq, Show)

instance Paginate ListPolicies

queryRequest ''ListPolicies

-- | Filters the result, showing only the escalation policies
-- whose names match the query.
lpQuery :: Lens' (Request ListPolicies s b) (Maybe Text)
lpQuery = upd.lpQuery'

-- | List all the existing escalation policies.
--
-- @GET \/escalation_policies@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/escalation_policies/list>
listPolicies :: Request ListPolicies s [Policy]
listPolicies =
    mk ListPolicies
        { _lpQuery' = Nothing
        } & path   .~ policies
          & unwrap .~ key "escalation_policies"

-- | List all the existing escalation policies with currently on-call users.
--
-- If the start and end of an on-call object are null, then the user is always
-- on-call for an escalation policy level.
--
-- @GET \/escalation_policies\/on_call@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/escalation_policies/on_call>
listOnCallPolicies :: Request ListPolicies s [Policy]
listOnCallPolicies = listPolicies & path .~ policies % "on_call"

data CreatePolicy = CreatePolicy
    { _cpName'            :: Text
    , _cpRepeatEnabled'   :: Bool'
    , _cpNumLoops'        :: Maybe Int
    , _cpEscalationRules' :: [Rule] -- ^ Should be List1
    } deriving (Eq, Show)

jsonRequest ''CreatePolicy

-- | The name of the escalation policy.
cpName :: Lens' (Request CreatePolicy s b) Text
cpName = upd.cpName'

-- | Whether or not to allow this policy to repeat its escalation
-- rules after the last rule is finished.
--
-- /Default:/ false.
cpRepeatEnabled :: Lens' (Request CreatePolicy s b) Bool
cpRepeatEnabled = upd.cpRepeatEnabled'._B

-- | The number of times to loop over the set of rules in this escalation policy.
cpNumLoops :: Lens' (Request CreatePolicy s b) (Maybe Int)
cpNumLoops = upd.cpNumLoops'

-- | The escalation rules for this policy.
cpEscalationRules :: Lens' (Request CreatePolicy s b) [Rule]
cpEscalationRules = upd.cpEscalationRules'

-- | Creates an existing escalation policy and rules.
--
-- @POST \/escalation_policies@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/escalation_policies/create>
createPolicy :: EscalationPolicyId
             -> Text   -- ^ 'creName'
             -> [Rule] -- ^ 'creEscalationRules'
             -> Request CreatePolicy s Policy
createPolicy i n rs =
    mk CreatePolicy
        { _cpName'            = n
        , _cpRepeatEnabled'   = F
        , _cpNumLoops'        = Nothing
        , _cpEscalationRules' = rs
        } & meth   .~ PUT
          & path   .~ policies % i
          & unwrap .~ key "escalation_policy"

-- | Get information about an existing escalation policy and its rules.
--
-- @GET \/escalation_policies\/\:id@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/escalation_policies/show>
getPolicy :: EscalationPolicyId -> Request Empty s Policy
getPolicy i = empty & path .~ policies % i & unwrap .~ key "escalation_policy"

data UpdatePolicy = UpdatePolicy
    { _upName'            :: Maybe Text
    , _upRepeatEnabled'   :: Maybe Bool'
    , _upNumLoops'        :: Maybe Int
    , _upEscalationRules' :: [Rule]
    } deriving (Eq, Show)

jsonRequest ''UpdatePolicy

-- | The name of the escalation policy.
upName :: Lens' (Request UpdatePolicy s b) (Maybe Text)
upName = upd.upName'

-- | Whether or not to allow this policy to repeat its escalation
-- rules after the last rule is finished.
upRepeatEnabled :: Lens' (Request UpdatePolicy s b) (Maybe Bool)
upRepeatEnabled = upd.upRepeatEnabled'.mapping _B

-- | The number of times to loop over the set of rules in this escalation policy.
upNumLoops :: Lens' (Request UpdatePolicy s b) (Maybe Int)
upNumLoops = upd.upNumLoops'

-- | The escalation rules for this policy.
upEscalationRules :: Lens' (Request UpdatePolicy s b) [Rule]
upEscalationRules = upd.upEscalationRules'

-- | Updates an existing escalation policy and rules.
--
-- @PUT \/escalation_policies\/\:id@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/escalation_policies/update>
updatePolicy :: EscalationPolicyId -> Request UpdatePolicy s Policy
updatePolicy i =
    mk UpdatePolicy
        { _upName'            = Nothing
        , _upRepeatEnabled'   = Nothing
        , _upNumLoops'        = Nothing
        , _upEscalationRules' = []
        } & meth   .~ PUT
          & path   .~ policies % i
          & unwrap .~ key "escalation_policy"

-- | Deletes an existing escalation policy and rules. The escalation policy
-- must not be in use by any services.
--
-- @DELETE \/escalation_policies\/\:id@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/escalation_policies/delete>
deletePolicy :: EscalationPolicyId -> Request Empty s Empty
deletePolicy i = empty & meth .~ DELETE & path .~ policies % i
