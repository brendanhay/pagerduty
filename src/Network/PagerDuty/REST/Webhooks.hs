{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Module      : Network.PagerDuty.REST.Webhooks
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Webhooks let you recieve HTTP callbacks when interesting events happen within
-- your PagerDuty account. Details surrounding the interesting event will be sent
-- via HTTP to a URL that you specify.
--
-- PagerDuty currently supports incident-based webhooks. After adding a webhook
-- URL to a PagerDuty service, the triggering of new incidents on that service
-- will cause outgoing webhook messages to be sent to that URL. In addition,
-- certain interesting changes to an incident's state will cause other types of
-- incident webhook messages to be sent: Generally, any change to the @status@ or
-- @assigned_to_user@ of an incident will cause an outgoing message to be sent.
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/webhooks>
module Network.PagerDuty.REST.Webhooks
    ( WebhookType (..)
    , Webhook
    , wId
    , wType
    , wCreatedOn
    ) where

import           Control.Applicative              hiding (empty)
import           Control.Lens
import           Data.Aeson
import           Data.ByteString.Conversion       (ToByteString(..), toByteString')
import           Data.Text                        (Text)
import qualified Data.Text.Encoding               as Text
import           Data.Time
import           Network.PagerDuty.REST.Incidents
import           Network.PagerDuty.Internal.TH
import           Network.PagerDuty.Internal.Types

data WebhookType
    = WTrigger
      -- ^ Sent when an incident is newly created/triggered.
    | WAcknowledge
      -- ^ Sent when an incident has had its status changed from triggered to
      -- acknowledged.
    | WUnacknowledge
      -- ^ Sent when an incident is unacknowledged due to timeout.
    | WResolve
      -- ^ Sent when an incident has been resolved.
    | WAssign
      -- ^ Sent when an incident has been manually reassigned to another user
      -- in a different escalation chain.
    | WEscalate
      -- ^ Sent when an incident has been escalated to another user in the
      -- same escalation chain.
    | WDelegate
      -- ^ Sent when an incident has been reassigned to another escalation chain.
    | WOther Text
      -- ^ /Note:/ As new types of incident actions are supported,
      -- new incident webhook message types will be added.
      deriving (Eq, Show)

instance ToByteString WebhookType where
    builder = \case
        WTrigger       -> "incident.trigger"
        WAcknowledge   -> "incident.acknowledge"
        WUnacknowledge -> "incident.unacknowledge"
        WResolve       -> "incident.resolve"
        WAssign        -> "incident.assign"
        WEscalate      -> "incident.escalate"
        WDelegate      -> "incident.delegate"
        WOther t       -> builder t

instance FromJSON WebhookType where
    parseJSON = withText "status" $ pure . \case
        "incident.trigger"       -> WTrigger
        "incident.acknowledge"   -> WAcknowledge
        "incident.unacknowledge" -> WUnacknowledge
        "incident.resolve"       -> WResolve
        "incident.assign"        -> WAssign
        "incident.escalate"      -> WEscalate
        "incident.delegate"      -> WDelegate
        t                        -> (WOther t)

instance ToJSON WebhookType where
    toJSON = String . Text.decodeUtf8 . toByteString'

data Webhook = Webhook
    { _wId        :: WebhookId
    , _wType      :: !WebhookType
    , _wCreatedOn :: !Date
    , _wData      :: Incident
    } deriving (Eq, Show)

deriveJSON ''Webhook

instance HasIncident Webhook where
    incident = lens _wData (\s x -> s { _wData = x })

-- | Uniquely identifies this outgoing webhook message; can be used for
-- idempotency when processing the messages.
makeLens "_wId" ''Webhook

-- | The webhook message type.
makeLens "_wType" ''Webhook

-- | The date/time when the incident changed state.
wCreatedOn :: Lens' Webhook UTCTime
wCreatedOn = lens _wCreatedOn (\i x -> i { _wCreatedOn = x }) . _D
