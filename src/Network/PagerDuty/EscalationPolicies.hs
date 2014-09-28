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

-- | Escalation Policies API
--
-- | This API lets you access and manipulate escalation policies and rules.
module Network.PagerDuty.EscalationPolicies where

import           Control.Applicative
import           Control.Lens               hiding ((.=))
import           Data.Aeson                 hiding (Error)
import           Data.Aeson.Lens
import qualified Data.ByteString.Char8      as BS
import           Data.ByteString.Conversion
import qualified Data.HashMap.Strict        as Map
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Network.HTTP.Types
import           Network.PagerDuty.TH
import           Network.PagerDuty.Types

-- dummy
data Service = Service
    deriving (Eq, Show)

deriveJSON ''Service

req :: (ToJSON a, ToByteString p)
    => StdMethod
    -> p
    -> Unwrap
    -> a
    -> Request a s r
req m p u = req' m ("escalation_policies", p) u

data ScheduleTarget = ScheduleTarget
    { _schedId   :: TargetId
    , _schedName :: Text
    } deriving (Eq, Show)

-- | The id of the target.
makeLens "_schedId" ''ScheduleTarget

-- | The name of the target.
makeLens "_schedName" ''ScheduleTarget

deriveJSON ''ScheduleTarget

data UserTarget = UserTarget
    { _userId       :: TargetId
    , _userName     :: Text
    , _userEmail    :: Email
    , _userTimeZone :: TimeZone
    , _userColor    :: Text
    } deriving (Eq, Show)

-- | The id of the user.
makeLens "_userId" ''UserTarget

-- | The name of the user.
makeLens "_userName" ''UserTarget

-- | The user's email address.
makeLens "_userEmail" ''UserTarget

-- | The user's personal time zone.
makeLens "_userTimeZone" ''UserTarget

-- | The color used to represent the user in schedules.
makeLens "_userColor" ''UserTarget

deriveJSON ''UserTarget

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

-- | The ID of the escalation rule.
makeLens "_ruleId" ''Rule

-- | The amount of time before an incident escalates away from this rule.
makeLens "_ruleEscalationDelayInMinutes" ''Rule

-- | A list of targets which an incident will be assigned to upon reaching this rule.
makeLens "_ruleTargets" ''Rule

deriveJSON ''Rule

data Policy = Policy
    { _policyId       :: PolicyId
      -- ^ The ID of the escalation policy.
    , _policyName     :: Text
      -- ^ The name of the escalation policy.
    , _policyNumLoops :: !Int
      -- ^ The number of times the escalation policy will repeat after
      -- reaching the end of its escalation.
    , _policyRules    :: [Rule]
      -- ^ A list of the policy's escalation rules in order of escalation.
    , _policyServices :: [Service]
      -- ^ A list of services using this escalation policy.
    } deriving (Eq, Show)

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

deriveJSON ''Policy

newtype ListPolicies = ListPolicies
    { _lstQuery :: Maybe Text
    } deriving (Eq, Show)

-- | List all the existing escalation policies.
listPolicies :: Request ListPolicies Token [Policy]
listPolicies = req GET BS.empty (key "escalation_policies") $
    ListPolicies
        { _lstQuery = Nothing
        }

-- | Filters the result, showing only the escalation policies
-- whose names match the query.
makeLens "_lstQuery" ''ListPolicies

deriveJSON ''ListPolicies

data CreatePolicy = CreatePolicy
    { _creName            :: Text
    , _creRepeatEnabled   :: Maybe Bool
    , _creNumLoops        :: Maybe Int
    , _creEscalationRules :: [Rule] -- ^ Should be List1
    } deriving (Eq, Show)

-- | Creates an existing escalation policy and rules.
createPolicy :: PolicyId
             -> Text   -- ^ 'creName'
             -> [Rule] -- ^ 'creEscalationRules'
             -> Request CreatePolicy Token Policy
createPolicy i n rs = req PUT i (key "escalation_policy") $
    CreatePolicy
        { _creName            = n
        , _creRepeatEnabled   = Nothing
        , _creNumLoops        = Nothing
        , _creEscalationRules = rs
        }

-- | The name of the escalation policy.
makeLens "_creName" ''CreatePolicy

-- | Whether or not to allow this policy to repeat its escalation
-- rules after the last rule is finished. Defaults to false.
makeLens "_creRepeatEnabled" ''CreatePolicy

-- | The number of times to loop over the set of rules in this escalation policy.
makeLens "_creNumLoops" ''CreatePolicy

-- | The escalation rules for this policy.
makeLens "_creEscalationRules" ''CreatePolicy

deriveJSON ''CreatePolicy

data GetPolicy = GetPolicy

deriveJSON ''GetPolicy

-- | Get information about an existing escalation policy and its rules.
getPolicy :: PolicyId -> Request GetPolicy Token Policy
getPolicy i = req GET i (key "escalation_policy") GetPolicy

data UpdatePolicy = UpdatePolicy
    { _updName            :: Maybe Text
    , _updRepeatEnabled   :: Maybe Bool
    , _updNumLoops        :: Maybe Int
    , _updEscalationRules :: [Rule]
    } deriving (Eq, Show)

-- | Updates an existing escalation policy and rules.
updatePolicy :: PolicyId -> Request UpdatePolicy Token Policy
updatePolicy i = req PUT i (key "escalation_policy") $
    UpdatePolicy
        { _updName            = Nothing
        , _updRepeatEnabled   = Nothing
        , _updNumLoops        = Nothing
        , _updEscalationRules = []
        }

-- | The name of the escalation policy.
makeLens "_updName" ''UpdatePolicy

-- | Whether or not to allow this policy to repeat its escalation
-- rules after the last rule is finished.
makeLens "_updRepeatEnabled" ''UpdatePolicy

-- | The number of times to loop over the set of rules in this escalation policy.
makeLens "_updNumLoops" ''UpdatePolicy

-- | The escalation rules for this policy.
makeLens "_updEscalationRules" ''UpdatePolicy

deriveJSON ''UpdatePolicy

data DeletePolicy = DeletePolicy

deriveJSON ''DeletePolicy

-- FIXME: Expected response 204 no content
-- | Deletes an existing escalation policy and rules. The escalation policy
-- must not be in use by any services.
deletePolicy :: PolicyId -> Request DeletePolicy Token ()
deletePolicy i = req DELETE i unwrap DeletePolicy
