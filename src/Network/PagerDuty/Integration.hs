{-# LANGUAGE GADTs             #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Module      : Network.PagerDuty.Integration
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The PagerDuty event integration API is how you would add PagerDuty's
-- advanced alerting functionality to any system that can make an HTTP API
-- call. You can now add phone, SMS and email alerting to your monitoring
-- tools, ticketing systems and custom software.
--
-- The API was designed to allow you to easily integrate a
-- monitoring system with a Service in PagerDuty. Monitoring systems
-- generally send out events when problems are detected and when these
-- problems have been resolved (fixed). Some more advanced systems also
-- understand the concept of acknowledgements: problems can be acknowledged
-- by an engineer to signal he or she is working on fixing the issue.
--
-- Since monitoring systems emit events, the API is based around accepting
-- events. Incoming events (sent via the API) are routed to a PagerDuty
-- service and processed. They may result in a new incident being created,
-- or an existing incident being acknowledged or resolved.
--
-- The same event-based API can also be used to integrate a PagerDuty
-- service with ticketing systems and various other software tools.
--
-- See: <http://developer.pagerduty.com/documentation/integration/events>
module Network.PagerDuty.Integration
    (
    -- * Events
      submit

    -- ** Trigger
    , Trigger
    , trigger
    , tDescription
    , tIncidentKey
    , tClient
    , tClientUrl
    , tDetails

    -- ** Acknowledge
    , Acknowledge
    , acknowledge
    , aDescription
    , aDetails

    -- ** Resolve
    , Resolve
    , resolve
    , rDescription
    , rDetails

    -- * Types
    , AnyEvent (..)

    , Response
    , rsStatus
    , rsMessage
    , rsIncidentKey

    , Error
    , ieStatus
    , ieMessage
    , ieErrors
    ) where

import           Control.Applicative
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict     as Map
import           Data.Monoid
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Network.PagerDuty.TH
import           Network.PagerDuty.Types (ServiceKey, IncidentKey)

data Response = Response
    { _rsStatus      :: Text
    , _rsMessage     :: Text
    , _rsIncidentKey :: IncidentKey
    } deriving (Eq, Show)

deriveJSON ''Response

-- | @success@.
makeLens "_rsStatus" ''Response

-- | @Event processed@.
makeLens "_rsMessage" ''Response

-- | The key of the incident that will be affected by the request.
makeLens "_rsIncidentKey" ''Response

data Error = Error
    { _ieStatus  :: Text
    , _ieMessage :: Text
    , _ieErrors  :: [Text]
    } deriving (Eq, Show)

deriveJSON ''Error

-- | @invalid event@
makeLens "_ieStatus" ''Error

-- | A description of the problem.
makeLens "_ieMessage" ''Error

-- | A list of specific error messages.
makeLens "_ieErrors" ''Error

class Event a where
    eventType    :: a -> Text
    eventPayload :: a -> Object

    default eventPayload :: ToJSON a => a -> Object
    eventPayload x = let Object o = toJSON x in o

data AnyEvent where
    Event :: Event a => a -> AnyEvent

instance Event AnyEvent where
    eventType    (Event x) = eventType x
    eventPayload (Event x) = eventPayload x

data Generic = Generic
    { _gServiceKey  :: ServiceKey
    , _gDescription :: Maybe Text
    , _gIncidentKey :: IncidentKey
    , _gDetails     :: Object
    } deriving (Eq, Show)

deriveJSON ''Generic
makeLenses ''Generic

data Trigger = Trigger
    { _tServiceKey  :: ServiceKey
    , _tDescription :: Text
    , _tIncidentKey :: Maybe IncidentKey
    , _tClient      :: Maybe Text
    , _tClientUrl   :: Maybe Text
    , _tDetails     :: Object
    } deriving (Eq, Show)

deriveJSON ''Trigger

instance Event Trigger where
    eventType = const "trigger"

-- | Your monitoring tools should send PagerDuty a trigger event to report a new
-- or ongoing problem. When PagerDuty receives a trigger event, it will either open
-- a new incident, or add a new trigger log entry to an existing incident,
-- depending on the provided incident_key.
trigger :: ServiceKey
        -> Text -- ^ 'tDescription'
        -> Trigger
trigger k d =
    Trigger
        { _tServiceKey  = k
        , _tDescription = d
        , _tIncidentKey = Nothing
        , _tClient      = Nothing
        , _tClientUrl   = Nothing
        , _tDetails     = mempty
        }

-- | A short description of the problem that led to this trigger.
-- This field (or a truncated version) will be used when generating phone
-- calls, SMS messages and alert emails. It will also appear on the incidents
-- tables in the PagerDuty UI. The maximum length is 1024 characters.
makeLens "_tDescription" ''Trigger

-- | Identifies the incident to which this trigger event should be applied.
-- If there's no open (i.e. unresolved) incident with this key, a new one
-- will be created. If there's already an open incident with a matching key,
-- this event will be appended to that incident's log. The event key provides
-- an easy way to "de-dup" problem reports. If this field isn't provided,
-- PagerDuty will automatically open a new incident with a unique key.
makeLens "_tIncidentKey" ''Trigger

-- | The name of the monitoring client that is triggering this event.
makeLens "_tClient" ''Trigger

-- | The URL of the monitoring client that is triggering this event.
makeLens "_tClientUrl" ''Trigger

-- | An arbitrary JSON object containing any data you'd like included
-- in the incident log.
makeLens "_tDetails" ''Trigger

newtype Acknowledge = Acknowledge Generic

deriveJSON ''Acknowledge
makePrisms ''Acknowledge

instance Event Acknowledge where
    eventType = const "acknowledge"

-- | Acknowledge events cause the referenced incident to enter the acknowledged
-- state.
--
-- While an incident is acknowledged, it won't generate any additional
-- notifications, even if it receives new trigger events. Your monitoring tools
-- should send PagerDuty an acknowledge event when they know someone is presently
-- working on the problem.
acknowledge :: ServiceKey -> IncidentKey -> Acknowledge
acknowledge k i =
    Acknowledge Generic
        { _gServiceKey  = k
        , _gDescription = Nothing
        , _gIncidentKey = i
        , _gDetails     = mempty
        }

-- | Text that will appear in the incident's log associated with this event.
aDescription :: Lens' Acknowledge (Maybe Text)
aDescription = _Acknowledge.gDescription

-- | An arbitrary JSON object containing any data you'd like included
-- in the incident log.
aDetails :: Lens' Acknowledge Object
aDetails = _Acknowledge.gDetails

newtype Resolve = Resolve Generic

deriveJSON ''Resolve
makePrisms ''Resolve

instance Event Resolve where
    eventType = const "acknowledge"

-- | Resolve events cause the referenced incident to enter the resolved state.
--
-- Once an incident is resolved, it won't generate any additional
-- notifications. New trigger events with the same incident_key as a resolved
-- incident won't re-open the incident. Instead, a new incident will be
-- created. Your monitoring tools should send PagerDuty a resolve event when the
-- problem that caused the initial trigger event has been fixed.
resolve :: ServiceKey -> IncidentKey -> Resolve
resolve k i =
    Resolve Generic
        { _gServiceKey  = k
        , _gDescription = Nothing
        , _gIncidentKey = i
        , _gDetails     = mempty
        }

-- | Text that will appear in the incident's log associated with this event.
rDescription :: Lens' Resolve (Maybe Text)
rDescription = _Resolve.gDescription

-- | An arbitrary JSON object containing any data youd like included
-- in the incident log.
rDetails :: Lens' Resolve Object
rDetails = _Resolve.gDetails

submit :: (Monad m, Event a) => a -> m (Either Error Response)
submit = undefined
  -- where
  --   payload = Map.insert "event_type" (String (eventType x)) (eventPayload x)
