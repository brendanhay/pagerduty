{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

-- Module      : Network.PagerDuty.Integration
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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
-- /See:/ <http://developer.pagerduty.com/documentation/integration/events>
module Network.PagerDuty.Integration
    (
    -- * Sending events
      submit
    , submitWith

    -- ** Trigger
    , Trigger
    , trigger
    , client
    , clientUrl

    -- ** Acknowledge
    , Acknowledge
    , acknowledge

    -- ** Resolve
    , Resolve
    , resolve

    -- * Fields
    , HasServiceKey  (..)
    , HasIncidentKey (..)
    , HasDescription (..)
    , HasDetails     (..)

    -- * Types
    , Event          (..)
    , _Trigger
    , _Acknowledge
    , _Resolve

    , Response
    , rsStatus
    , rsMessage
    , rsIncidentKey

    , Generic

    , module Network.PagerDuty.Types
    ) where

import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Default.Class
import qualified Data.HashMap.Strict              as Map
import           Data.Text                        (Text)
import           Network.HTTP.Client              (Manager)
import qualified Network.HTTP.Client              as Client
import           Network.PagerDuty.Internal.IO
import           Network.PagerDuty.Internal.TH
import           Network.PagerDuty.Internal.Types
import           Network.PagerDuty.Types          hiding (description, message)

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

class HasServiceKey s a | s -> a where
    -- | The GUID of one of your "Generic API" services. This is the "service key"
    -- listed on a Generic API's service detail page.
    serviceKey :: Lens' s a

class HasIncidentKey s a | s -> a where
    -- | Identifies the incident to which this trigger event should be applied.
    -- If there's no open (i.e. unresolved) incident with this key, a new one
    -- will be created. If there's already an open incident with a matching key,
    -- this event will be appended to that incident's log. The event key provides
    -- an easy way to "de-dup" problem reports. If this field isn't provided,
    -- PagerDuty will automatically open a new incident with a unique key.
    incidentKey :: Lens' s a

class HasDescription s a | s -> a where
    -- | A short description of the problem that led to this trigger.
    -- This field (or a truncated version) will be used when generating phone
    -- calls, SMS messages and alert emails. It will also appear on the incidents
    -- tables in the PagerDuty UI. The maximum length is 1024 characters.
    description :: Lens' s a

class HasDetails s a | s -> a where
    -- | An arbitrary JSON object containing any data you'd like included
    -- in the incident log.
    details :: Lens' s a

data Trigger = Trigger'
    { _tServiceKey'  :: ServiceKey
    , _tIncidentKey' :: IncidentKey
    , _tDescription' :: Text
    , _tDetails'     :: Maybe Object
    , _tClient'      :: Maybe Text
    , _tClientUrl'   :: Maybe Text
    } deriving (Eq, Show)

deriveRecord ''Trigger

instance HasServiceKey  Trigger ServiceKey     where serviceKey  = tServiceKey'
instance HasIncidentKey Trigger IncidentKey    where incidentKey = tIncidentKey'
instance HasDescription Trigger Text           where description = tDescription'
instance HasDetails     Trigger (Maybe Object) where details     = tDetails'

-- | The name of the monitoring client that is triggering this event.
client :: Lens' Trigger (Maybe Text)
client = tClient'

-- | The URL of the monitoring client that is triggering this event.
clientUrl :: Lens' Trigger (Maybe Text)
clientUrl = tClientUrl'

data Generic = Generic'
    { _gServiceKey'  :: ServiceKey
    , _gIncidentKey' :: IncidentKey
    , _gDescription' :: Maybe Text
    , _gDetails'     :: Maybe Object
    } deriving (Eq, Show)

deriveRecord ''Generic

instance HasServiceKey  Generic ServiceKey     where serviceKey  = gServiceKey'
instance HasIncidentKey Generic IncidentKey    where incidentKey = gIncidentKey'
instance HasDescription Generic (Maybe Text)   where description = gDescription'
instance HasDetails     Generic (Maybe Object) where details     = gDetails'

type Resolve     = Generic
type Acknowledge = Generic

data Event
    = Trigger     Trigger -- ^ /See:/ 'trigger'
    | Acknowledge Generic -- ^ /See:/ 'acknowledge'
    | Resolve     Generic -- ^ /See:/ 'resolve'
      deriving (Eq, Show)

makePrisms ''Event

instance HasServiceKey Event ServiceKey where
    serviceKey = lens f g
      where
        f (Trigger     s) = _tServiceKey' s
        f (Acknowledge s) = _gServiceKey' s
        f (Resolve     s) = _gServiceKey' s

        g (Trigger     s) x = Trigger     $ s { _tServiceKey' = x }
        g (Acknowledge s) x = Acknowledge $ s { _gServiceKey' = x }
        g (Resolve     s) x = Resolve     $ s { _gServiceKey' = x }

instance HasIncidentKey Event IncidentKey where
    incidentKey = lens f g
      where
        f (Trigger     s) = _tIncidentKey' s
        f (Acknowledge s) = _gIncidentKey' s
        f (Resolve     s) = _gIncidentKey' s

        g (Trigger     s) x = Trigger     $ s { _tIncidentKey' = x }
        g (Acknowledge s) x = Acknowledge $ s { _gIncidentKey' = x }
        g (Resolve     s) x = Resolve     $ s { _gIncidentKey' = x }

instance HasDetails Event (Maybe Object) where
    details = lens f g
      where
        f (Trigger     s) = _tDetails' s
        f (Acknowledge s) = _gDetails' s
        f (Resolve     s) = _gDetails' s

        g (Trigger     s) x = Trigger     $ s { _tDetails' = x }
        g (Acknowledge s) x = Acknowledge $ s { _gDetails' = x }
        g (Resolve     s) x = Resolve     $ s { _gDetails' = x }

instance ToJSON Event where
    toJSON = \case
        Trigger     x -> event "trigger"     x
        Acknowledge x -> event "acknowledge" x
        Resolve     x -> event "resolve"     x
      where
        event k x =
            case toJSON x of
                Object o -> Object (Map.insert "event_type" (String k) o)
                v        -> v

-- | Your monitoring tools should send PagerDuty a trigger event to report a new
-- or ongoing problem. When PagerDuty receives a trigger event, it will either open
-- a new incident, or add a new trigger log entry to an existing incident,
-- depending on the provided incident_key.
trigger :: ServiceKey   -- ^ 'serviceKey'
        -> IncidentKey  -- ^ 'incidentKey'
        -> Text         -- ^ 'description'
        -> Maybe Object -- ^ 'details'
        -> Event
trigger k i d t =
    Trigger Trigger'
        { _tServiceKey'  = k
        , _tDescription' = d
        , _tIncidentKey' = i
        , _tClient'      = Nothing
        , _tClientUrl'   = Nothing
        , _tDetails'     = t
        }

-- | Acknowledge events cause the referenced incident to enter the acknowledged
-- state.
--
-- While an incident is acknowledged, it won't generate any additional
-- notifications, even if it receives new trigger events. Your monitoring tools
-- should send PagerDuty an acknowledge event when they know someone is
-- presently working on the problem.
acknowledge :: ServiceKey  -- ^ 'serviceKey'
            -> IncidentKey -- ^ 'incidentKey'
            -> Event
acknowledge k i =
    Acknowledge Generic'
        { _gServiceKey'  = k
        , _gIncidentKey' = i
        , _gDescription' = Nothing
        , _gDetails'     = Nothing
        }

-- | Resolve events cause the referenced incident to enter the resolved state.
--
-- Once an incident is resolved, it won't generate any additional
-- notifications. New trigger events with the same incident_key as a resolved
-- incident won't re-open the incident. Instead, a new incident will be
-- created. Your monitoring tools should send PagerDuty a resolve event when the
-- problem that caused the initial trigger event has been fixed.
resolve :: ServiceKey  -- ^ 'serviceKey'
        -> IncidentKey -- ^ 'incidentKey'
        -> Event
resolve k i =
    Resolve Generic'
        { _gServiceKey'  = k
        , _gIncidentKey' = i
        , _gDescription' = Nothing
        , _gDetails'     = Nothing
        }

-- | Send an event to the integration API.
submit :: MonadIO m => Manager -> Event -> m (Either Error Response)
submit m = submitWith m None

-- | /See:/ 'submit'
submitWith :: MonadIO m
           => Manager
           -> Logger
           -> Event
           -> m (Either Error Response)
submitWith m l e = request m l e $
    def { Client.host   = "events.pagerduty.com"
        , Client.path   = "/generic/2010-04-15/create_event.json"
        , Client.method = "POST"
        }
