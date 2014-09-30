{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

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
-- See: <http://developer.pagerduty.com/documentation/rest/escalation_policies>
module Network.PagerDuty.REST.EscalationPolicies
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

import           Control.Applicative             ((<$>))
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.HashMap.Strict             as Map
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           Network.HTTP.Types
import           Network.PagerDuty.REST.Services (Service)
import           Network.PagerDuty.TH
import           Network.PagerDuty.Types

policies :: Path
policies = "escalation_policies"

data ScheduleTarget = ScheduleTarget
    { _stgtId   :: ScheduleId
    , _stgtName :: Text
    } deriving (Eq, Show)

deriveJSON ''ScheduleTarget

-- | The id of the target.
makeLens "_stgtId" ''ScheduleTarget

-- | The name of the target.
makeLens "_stgtName" ''ScheduleTarget

data UserTarget = UserTarget
    { _utgtId       :: UserId
    , _utgtName     :: Text
    , _utgtEmail    :: Email
    , _utgtTimeZone :: TimeZone
    , _utgtColor    :: Text
    } deriving (Eq, Show)

deriveJSON ''UserTarget

-- | The id of the user.
makeLens "_utgtId" ''UserTarget

-- | The name of the user.
makeLens "_utgtName" ''UserTarget

-- | The user's email address.
makeLens "_utgtEmail" ''UserTarget

-- | The user's personal time zone.
makeLens "_utgtTimeZone" ''UserTarget

-- | The color used to represent the user in schedules.
makeLens "_utgtColor" ''UserTarget

data Target
    = TSchedule ScheduleTarget
    | TUser     UserTarget
      deriving (Eq, Show)

-- type: A representation of the type of the target.
-- Will be either schedule or user.
instance FromJSON Target where
    parseJSON = withObject "target" $ \o -> do
        t <- o .: "type"
        case t of
            "schedule" -> f o TSchedule
            "user"     -> f o TUser
            _          -> fail $ "Unrecognised target type: " ++ Text.unpack t
      where
        f o g = g <$> parseJSON (Object o)

instance ToJSON Target where
    toJSON t = Object (Map.insert "type" (String k) o)
      where
        (k, Object o) = case t of
            TSchedule s -> ("schedule", toJSON s)
            TUser     u -> ("user",     toJSON u)

makePrisms ''Target

data Rule = Rule
    { _ruleId                       :: RuleId
    , _ruleEscalationDelayInMinutes :: !Int
    , _ruleTargets                  :: [Target]
    } deriving (Eq, Show)

deriveJSON ''Rule

-- | The ID of the escalation rule.
makeLens "_ruleId" ''Rule

-- | The amount of time before an incident escalates away from this rule.
makeLens "_ruleEscalationDelayInMinutes" ''Rule

-- | A list of targets which an incident will be assigned to upon reaching this rule.
makeLens "_ruleTargets" ''Rule

data Policy = Policy
    { _policyId       :: PolicyId
    , _policyName     :: Text
    , _policyNumLoops :: !Int
    , _policyRules    :: [Rule]
    , _policyServices :: [Service]
    } deriving (Eq, Show)

deriveJSON ''Policy

-- | The ID of the escalation policy.
makeLens "_policyId" ''Policy

-- | The name of the escalation policy.
makeLens "_policyName" ''Policy

-- | The number of times the escalation policy will repeat after
-- reaching the end of its escalation.
makeLens "_policyNumLoops" ''Policy

-- | A list of the policy's escalation rules in order of escalation.
makeLens "_policyRules" ''Policy

-- | A list of services using this escalation policy.
makeLens "_policyServices" ''Policy

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
listPolicies :: Request ListPolicies s [Policy]
listPolicies =
    mk ListPolicies
        { _lpQuery' = Nothing
        } & path   .~ policies
          & unwrap .~ key "escalation_policies"

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
             -> Request CreatePolicy s Policy
createPolicy i n rs =
    mk CreatePolicy
        { _cpName'            = n
        , _cpRepeatEnabled'   = Nothing
        , _cpNumLoops'        = Nothing
        , _cpEscalationRules' = rs
        } & meth   .~ PUT
          & path   .~ policies % i
          & unwrap .~ key "escalation_policy"

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
getPolicy :: PolicyId -> Request Empty s Policy
getPolicy i = empty & path .~ policies % i & unwrap .~ key "escalation_policy"

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
updatePolicy :: PolicyId -> Request UpdatePolicy s Policy
updatePolicy i =
    mk UpdatePolicy
        { _upName'            = Nothing
        , _upRepeatEnabled'   = Nothing
        , _upNumLoops'        = Nothing
        , _upEscalationRules' = []
        } & meth   .~ PUT
          & path   .~ policies % i
          & unwrap .~ key "escalation_policy"

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
deletePolicy :: PolicyId -> Request Empty s Empty
deletePolicy i = empty & meth .~ DELETE & path .~ policies % i
