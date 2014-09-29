{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Module      : Network.PagerDuty.EscalationPolicies.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.PagerDuty.EscalationPolicies.Types where

import           Control.Applicative
import           Control.Lens.TH
import           Data.Aeson
import qualified Data.HashMap.Strict              as Map
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import           Network.PagerDuty.Services.Types (Service)
import           Network.PagerDuty.TH
import           Network.PagerDuty.Types

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
