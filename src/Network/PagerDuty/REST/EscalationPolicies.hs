{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
-- _See:_ <http://developer.pagerduty.com/documentation/rest/escalation_policies>
module Network.PagerDuty.REST.EscalationPolicies
    (
    -- * List Policies
      ListPolicies
    , listPolicies
    , lpQuery

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
    , Target (..)
    , _TSchedule
    , _TUser

    , ScheduleTarget
    , stId
    , stName

    , UserTarget
    , utId
    , utName
    , utEmail
    , utTimeZone
    , utColor

    , Rule
    , rId
    , rEscalationDelayInMinutes
    , rTargets

    , Policy
    , pId
    , pName
    , pNumLoops
    , pRules
    , pServices
    ) where

import           Control.Applicative             ((<$>))
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.HashMap.Strict             as Map
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           Data.Time
import           Network.HTTP.Types
import           Network.PagerDuty.REST.Services (Service)
import           Network.PagerDuty.TH
import           Network.PagerDuty.Types

policies :: Path
policies = "escalation_policies"

data ScheduleTarget = ScheduleTarget
    { _stId   :: ScheduleId
    , _stName :: Text
    } deriving (Eq, Show)

deriveJSON ''ScheduleTarget

-- | The id of the target.
makeLens "_stId" ''ScheduleTarget

-- | The name of the target.
makeLens "_stName" ''ScheduleTarget

data UserTarget = UserTarget
    { _utId       :: UserId
    , _utName     :: Text
    , _utEmail    :: Address
    , _utTimeZone :: TZ
    , _utColor    :: Text
    } deriving (Eq, Show)

deriveJSON ''UserTarget

-- | The id of the user.
makeLens "_utId" ''UserTarget

-- | The name of the user.
makeLens "_utName" ''UserTarget

-- | The user's email address.
makeLens "_utEmail" ''UserTarget

-- | The user's personal time zone.
utTimeZone :: Lens' UserTarget TimeZone
utTimeZone = lens _utTimeZone (\t x -> t { _utTimeZone = x }) . _TZ

-- | The color used to represent the user in schedules.
makeLens "_utColor" ''UserTarget

data Target
    = TSchedule ScheduleTarget
    | TUser     UserTarget
      deriving (Eq, Show)

makePrisms ''Target

-- type: A representation of the type of the target.
-- Will be either schedule or user.
instance FromJSON Target where
    parseJSON = withObject "target" $ \o -> do
        t <- o .: "type"
        case t of
            "schedule" -> TSchedule <$> parseJSON (Object o)
            "user"     -> TUser     <$> parseJSON (Object o)
            _          -> fail $ "Unrecognised target type: " ++ Text.unpack t

instance ToJSON Target where
    toJSON t = Object (Map.insert "type" (String k) o)
      where
        (k, Object o) = case t of
            TSchedule s -> ("schedule", toJSON s)
            TUser     u -> ("user",     toJSON u)

data Rule = Rule
    { _rId                       :: RuleId
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

data Policy = Policy
    { _pId       :: PolicyId
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

deriveQuery ''ListPolicies

-- | Filters the result, showing only the escalation policies
-- whose names match the query.
lpQuery :: Lens' (Request ListPolicies s b) (Maybe Text)
lpQuery = upd.lpQuery'

-- | List all the existing escalation policies.
--
-- @GET \/escalation_policies@
--
-- _See:_ <http://developer.pagerduty.com/documentation/rest/escalation_policies/list>
listPolicies :: Request ListPolicies s [Policy]
listPolicies =
    mk ListPolicies
        { _lpQuery' = Nothing
        } & path   .~ policies
          & unwrap .~ key "escalation_policies"

data CreatePolicy = CreatePolicy
    { _cpName'            :: Text
    , _cpRepeatEnabled'   :: Bool'
    , _cpNumLoops'        :: Maybe Int
    , _cpEscalationRules' :: [Rule] -- ^ Should be List1
    } deriving (Eq, Show)

deriveBody ''CreatePolicy

-- | The name of the escalation policy.
cpName :: Lens' (Request CreatePolicy s b) Text
cpName = upd.cpName'

-- | Whether or not to allow this policy to repeat its escalation
-- rules after the last rule is finished.
--
-- _Default:_ false.
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
-- _See:_ <http://developer.pagerduty.com/documentation/rest/escalation_policies/create>
createPolicy :: PolicyId
             -> Text   -- ^ 'creName'
             -> [Rule] -- ^ 'creEscalationRules'
             -> Request CreatePolicy s Policy
createPolicy i n rs =
    mk CreatePolicy
        { _cpName'            = n
        , _cpRepeatEnabled'   = B False
        , _cpNumLoops'        = Nothing
        , _cpEscalationRules' = rs
        } & meth   .~ PUT
          & path   .~ policies % i
          & unwrap .~ key "escalation_policy"

-- | Get information about an existing escalation policy and its rules.
--
-- @GET \/escalation_policies\/\:id@
--
-- _See:_ <http://developer.pagerduty.com/documentation/rest/escalation_policies/show>
getPolicy :: PolicyId -> Request Empty s Policy
getPolicy i = empty & path .~ policies % i & unwrap .~ key "escalation_policy"

data UpdatePolicy = UpdatePolicy
    { _upName'            :: Maybe Text
    , _upRepeatEnabled'   :: Maybe Bool'
    , _upNumLoops'        :: Maybe Int
    , _upEscalationRules' :: [Rule]
    } deriving (Eq, Show)

deriveBody ''UpdatePolicy

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
-- _See:_ <http://developer.pagerduty.com/documentation/rest/escalation_policies/update>
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

-- | Deletes an existing escalation policy and rules. The escalation policy
-- must not be in use by any services.
--
-- @DELETE \/escalation_policies\/\:id@
--
-- _See:_ <http://developer.pagerduty.com/documentation/rest/escalation_policies/delete>
deletePolicy :: PolicyId -> Request Empty s Empty
deletePolicy i = empty & meth .~ DELETE & path .~ policies % i
