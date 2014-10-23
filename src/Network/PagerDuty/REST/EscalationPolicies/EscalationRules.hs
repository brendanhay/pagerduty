{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- Module      : Network.PagerDuty.REST.EscalationPolicies.EscalationRules
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This API lets you access and manipulate escalation rules of an existing
-- escalation policy.
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/escalation_policies/escalation_rules>
module Network.PagerDuty.REST.EscalationPolicies.EscalationRules
    (
    -- * List Rules
      listRules

    -- * Create Rule
    , CreateRule
    , createRule
    , crEscalationDelayInMinutes
    , crTargets

    -- * Get Rule
    , getRule

    -- * Update Rules
    , UpdateRules
    , updateRules
    , urEscalationRules

    -- * Update Rule
    , UpdateRule
    , updateRule
    , urEscalationDelayInMinutes
    , urTargets

    -- * Delete Rule
    , deleteRule

    -- * Types
    , HasUserInfo (..)

    , Target      (..)
    , _TSchedule
    , _TUser

    , ScheduleTarget
    , stId
    , stName

    , Rule
    , rId
    , rEscalationDelayInMinutes
    , rTargets
    ) where

import           Control.Applicative          ((<$>))
import           Control.Lens                 hiding ((.=))
import           Data.Aeson
import qualified Data.HashMap.Strict          as Map
import           Data.Text                    (Text)
import           Network.HTTP.Types
import           Network.PagerDuty.REST.Users
import           Network.PagerDuty.TH
import           Network.PagerDuty.Types

default (Path)

-- FIXME: The lenses here need to operate over (Request x s b) using upd

rules :: EscalationPolicyId -> Path
rules p = "escalation_policies" % p % "escalation_rules"

data TargetType
    = TargetSchedule
    | TargetUser

deriveJSONWith (dropped 6 underscored) ''TargetType

data TargetId
    = ScheduleId ScheduleId
    | UserId     UserId
      deriving (Eq, Show)

instance FromJSON TargetId where
    parseJSON = withObject "target" $ \o -> do
        t <- o .: "type"
        case t of
            TargetSchedule -> ScheduleId <$> o .: "id"
            TargetUser     -> UserId     <$> o .: "id"

instance ToJSON TargetId where
    toJSON (ScheduleId s) = object ["type" .= TargetSchedule, "id" .= s]
    toJSON (UserId     u) = object ["type" .= TargetUser,     "id" .= u]

data ScheduleTarget = ScheduleTarget
    { _stId   :: ScheduleId
    , _stName :: Text
    } deriving (Eq, Show)

deriveJSON ''ScheduleTarget

-- | The id of the target.
makeLens "_stId" ''ScheduleTarget

-- | The name of the target.
makeLens "_stName" ''ScheduleTarget

data Target
    = TSchedule ScheduleTarget
    | TUser     UserInfo
      deriving (Eq, Show)

makePrisms ''Target

instance FromJSON Target where
    parseJSON = withObject "target" $ \o -> do
        t <- o .: "type"
        case t of
            TargetSchedule -> TSchedule <$> parseJSON (Object o)
            TargetUser     -> TUser     <$> parseJSON (Object o)

instance ToJSON Target where
    toJSON t = Object (Map.insert "type" (String k) o)
      where
        (k, Object o) = case t of
            TSchedule s -> ("schedule", toJSON s)
            TUser     u -> ("user",     toJSON u)

data Rule = Rule
    { _rId                       :: EscalationRuleId
    , _rEscalationDelayInMinutes :: !Int
    , _rTargets                  :: [Target]
    } deriving (Eq, Show)

deriveJSON ''Rule

-- | The ID of the escalation rule.
makeLens "_rId" ''Rule

-- | The amount of time before an incident escalates away from this rule.
makeLens "_rEscalationDelayInMinutes" ''Rule

-- | A list of targets which an incident will be assigned to upon reaching this rule.
makeLens "_rTargets" ''Rule

-- | List all the escalation rules for an existing escalation policy.
--
-- @GET \/escalation_policies\/\:escalation_policy_id\/escalation_rules@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/escalation_policies/escalation_rules/list>
listRules :: EscalationPolicyId -> Request Empty s [Rule]
listRules p = empty & path .~ rules p

-- | Show the escalation rule for an existing escalation policy.
--
-- @GET \/escalation_policies\/:escalation_policy_id\/escalation_rules\/\:id@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/escalation_policies/escalation_rules/show>
getRule :: EscalationPolicyId -> EscalationRuleId -> Request Empty s Rule
getRule p r = empty & path .~ rules p % r

data CreateRule = CreateRule
    { _crEscalationDelayInMinutes :: !Int
    , _crTargets                  :: [TargetId] -- ^ list1?
    } deriving (Eq, Show)

deriveJSON ''CreateRule

instance QueryLike CreateRule where
    toQuery = const []

-- | The escalation timeout in minutes. Must be at least 5 if the rule has
-- multiple targets, and at least 1 if not. If an incident is not acknowledged
-- within this timeout then it will escalate onto the next escalation rule.
makeLens "_crEscalationDelayInMinutes" ''CreateRule

-- | The list of the targets an incident should be assigned to upon reaching
-- this rule.
makeLens "_crTargets" ''CreateRule

-- | Creates a new escalation rule for an escalation policy and appends it to
-- the end of the existing escalation rules.
--
-- @POST \/escalation_policies\/\:escalation_policy_id\/escalation_rules@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/escalation_policies/escalation_rules/create>
createRule :: EscalationPolicyId
           -> Int                        -- ^ 'crEscalationDelayInMinutes'
           -> [TargetId] -- ^ 'crTargets'
           -> Request CreateRule s Rule
createRule p n ts =
    mk CreateRule
        { _crEscalationDelayInMinutes = n
        , _crTargets                  = ts
        } & meth .~ POST
          & path .~ rules p

newtype UpdateRules = UpdateRules
    { _urEscalationRules :: [TargetId]  -- ^ list1
    } deriving (Eq, Show)

deriveJSON ''UpdateRules

instance QueryLike UpdateRules where
    toQuery = const []

-- | An ordered array of escalation rules.
makeLens "_urEscalationRules" ''UpdateRules

-- | Updates the entire collection of escalation rules for an existing escalation
-- policy. The ordering is determined by the positions of each rule in the
-- list. To create a new rule, add it to the list without an id. To remove a
-- rule, do not include it in the list.
--
-- @PUT \/escalation_policies\/\:escalation_policy_id\/escalation_rules@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/escalation_policies/escalation_rules/multi_update>
updateRules :: EscalationPolicyId
            -> [TargetId] -- ^ 'urEscalationRules'
            -> Request UpdateRules s [Rule]
updateRules p rs =
    mk UpdateRules
        { _urEscalationRules = rs
        } & meth .~ PUT
          & path .~ rules p

data UpdateRule = UpdateRule
    { _urEscalationDelayInMinutes :: Maybe Int
    , _urTargets                  :: [TargetId]
    } deriving (Eq, Show)

deriveJSON ''UpdateRule

instance QueryLike UpdateRule where
    toQuery = const []

-- | The escalation timeout in minutes. Must be at least 5 if the rule has
-- multiple targets, and at least 1 if not. If an incident is not acknowledged
-- within this timeout then it will escalate onto the next escalation rule.
makeLens "_urEscalationDelayInMinutes" ''UpdateRule

-- | The list of the targets an incident should be assigned to upon reaching
-- this rule.
makeLens "_urTargets" ''UpdateRule

-- | Updates an escalation rule.
--
-- @PUT \/escalation_policies\/\:escalation_policy_id\/escalation_rules\/\:id@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/escalation_policies/escalation_rules/update>
updateRule :: EscalationPolicyId -> EscalationRuleId -> Request UpdateRule s Rule
updateRule p r =
    mk UpdateRule
        { _urEscalationDelayInMinutes = Nothing
        , _urTargets                  = []
        } & meth .~ PUT
          & path .~ rules p % r

-- | Deletes an existing escalation rule that belongs to an escalation
-- policy. There must be a matching rule.
--
-- @DELETE \/escalation_policies\/\:escalation_policy_id\/escalation_rules\/\:id@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/escalation_policies/escalation_rules/delete>
deleteRule :: EscalationPolicyId -> EscalationRuleId -> Request Empty s Empty
deleteRule p r = empty & meth .~ DELETE & path .~ rules p % r
