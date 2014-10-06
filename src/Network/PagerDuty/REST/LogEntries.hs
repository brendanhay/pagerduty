{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.PagerDuty.REST.LogEntries
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | PagerDuty keeps a log of all the events that happen to an incident. The
-- following APIs provide fine-grained access to this incident log entry data to
-- give you more insight into how your team or organization is handling your
-- incidents. Log entry data includes details about the event(s) that triggered
-- the incident, who was notified and when, how they were notified, and who
-- acknowledged or resolved it, amongst a few other things.
module Network.PagerDuty.REST.LogEntries
    (
    -- * List Logs
      ListLogs
    , listLogs
    , listUserLogs
    , listIncidentLogs
    , llTimeZone
    , llSince
    , llUntil
    , llIsOverview

    -- * Get Log
    , GetLog
    , getLog
    , glTimeZone

    -- * Types
    -- ** Channels
    , Channel (..)
    , _ChanNagios
    , _ChanAPI
    , _ChanEmail
    , _ChanUserEmail
    , _ChanWebTrigger
    , _ChanSMS
    , _ChanWeb
    , _ChanNote
    , _ChanAuto
    , _ChanTimeout

    , NagiosChannel
    , ncSummary
    , ncHost
    , ncService
    , ncState
    , ncDetails

    , APIChannel
    , acSummary
    , acServiceKey
    , acDescription
    , acIncidentKey
    , acDetails

    , EmailType (..)
    , EmailChannel
    , ecSummary
    , ecTo
    , ecFrom
    , ecSubject
    , ecBody
    , ecBodyContentType
    , ecRawUrl
    , ecHtmlUrl

    , WebTriggerChannel
    , wcSummary
    , wcSubject
    , wcDetails

    -- ** Entries
    , Entry (..)
    , _LogEntry
    , _NotifyEntry

    , LogEntryType (..)
    , LogEntry
    , leId
    , leType
    , leCreatedAt
    , leNote
    , leAssignedUser
    , leChannel

    , NotifyEntry
    , neCreatedAt
    , neUser
    , neNotification

    , NotificationType   (..)
    , NotificationStatus (..)

    , Notification
    , nType
    , nStatus
    , nAddress
    ) where

import           Control.Applicative
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Default
import qualified Data.HashMap.Strict          as Map
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Data.Time
import           Network.HTTP.Types
import           Network.PagerDuty.REST.Users (User)
import           Network.PagerDuty.TH
import           Network.PagerDuty.Types

logs :: Path
logs = "log_entries"

includes :: Query
includes =
    [ ("include[]", Just "channel")
    , ("include[]", Just "incident")
    , ("include[]", Just "service")
    ]

data NagiosChannel = NagiosChannel
    { _ncSummary :: Text
    , _ncHost    :: Text
    , _ncService :: Maybe Text
    , _ncState   :: Text
    , _ncDetails :: Object
    } deriving (Eq, Show)

deriveJSON ''NagiosChannel

-- | Same as host.
makeLens "_ncSummary" ''NagiosChannel

-- | Nagios host.
makeLens "_ncHost" ''NagiosChannel

-- | Nagios service that created the event, if applicable.
makeLens "_ncService" ''NagiosChannel

-- | State the caused the event.
makeLens "_ncState" ''NagiosChannel

-- | Additional details of the incident.
makeLens "_ncDetails" ''NagiosChannel

data APIChannel = APIChannel
    { _acSummary     :: Text
    , _acServiceKey  :: ServiceKey
    , _acDescription :: Text
    , _acIncidentKey :: IncidentKey
    , _acDetails     :: Object
    } deriving (Eq, Show)

deriveJSON ''APIChannel

-- | Same as description.
makeLens "_acSummary" ''APIChannel

-- | API service key.
makeLens "_acServiceKey" ''APIChannel

-- | Description of the event.
makeLens "_acDescription" ''APIChannel

-- | Incident deduping string.
makeLens "_acIncidentKey" ''APIChannel

-- | Additional details of the incident, if any.
makeLens "_acDetails" ''APIChannel

data EmailType
    = Plain
    | HTML
      deriving (Eq, Show)

deriveNullary ''EmailType

data EmailChannel = EmailChannel
    { _ecSummary         :: Text
    , _ecTo              :: Text
    , _ecFrom            :: Text
    , _ecSubject         :: Text
    , _ecBody            :: Text
    , _ecBodyContentType :: EmailType
    , _ecRawUrl          :: Text
    , _ecHtmlUrl         :: Maybe Text
    } deriving (Eq, Show)

deriveJSON ''EmailChannel

-- | Same as subject.
makeLens "_ecSummary" ''EmailChannel

-- | To address of the email.
makeLens "_ecTo" ''EmailChannel

-- | From address of the email.
makeLens "_ecFrom" ''EmailChannel

-- | Subject of the email.
makeLens "_ecSubject" ''EmailChannel

-- | Body of the email.
makeLens "_ecBody" ''EmailChannel

-- | Content type of the email body.
makeLens "_ecBodyContentType" ''EmailChannel

-- | URL for raw text of email.
makeLens "_ecRawUrl" ''EmailChannel

-- | URL for html rendered version of the email.
--
-- Only present if content_type is 'HTML'.
makeLens "_ecHtmlUrl" ''EmailChannel

data WebTriggerChannel = WebTriggerChannel
    { _wcSummary :: Text
    , _wcSubject :: Text
    , _wcDetails :: Maybe Text
    } deriving (Eq, Show)

deriveJSON ''WebTriggerChannel

-- | Same as subject.
makeLens "_wcSummary" ''WebTriggerChannel

-- | Subject of the web trigger.
makeLens "_wcSubject" ''WebTriggerChannel

-- | Details about the web trigger.
makeLens "_wcDetails" ''WebTriggerChannel

-- | Representation of the means by which the action was channeled.
data Channel
    = ChanNagios          NagiosChannel
    | ChanAPI             APIChannel
    | ChanEmail           EmailChannel
    | ChanUserEmail  User EmailChannel
    | ChanWebTrigger User WebTriggerChannel
    | ChanSMS        User
    | ChanWeb        User
    | ChanNote       User
    | ChanAuto
    | ChanTimeout
      deriving (Eq, Show)

makePrisms ''Channel

instance FromJSON Channel where
    parseJSON = withObject "agent+channel" $ \o -> do
         t <- o .: "channel" >>= (.: "type")
         let u = Map.member "user" o
         case t of
         -- service
            "email" | not u -> ChanEmail      <$> o .: "channel"
            "nagios"        -> ChanNagios     <$> o .: "channel"
            "api"           -> ChanAPI        <$> o .: "channel"
         -- user
            "email" | u     -> ChanUserEmail  <$> o .: "user" <*> o .: "channel"
            "web_trigger"   -> ChanWebTrigger <$> o .: "user" <*> o .: "channel"
            "sms"           -> ChanSMS        <$> o .: "user"
            "web"           -> ChanWeb        <$> o .: "user"
            "note"          -> ChanNote       <$> o .: "user"
         -- empty
            "auto"          -> pure ChanAuto
            "timeout"       -> pure ChanTimeout
            _               -> fail $
                "unrecognised service channel type: " ++ Text.unpack t

data LogEntryType
    = Trigger
      -- ^ The incident was triggered.
    | Acknowledge
      -- ^ The incident was acknowledged.
    | Unacknowledge
      -- ^ The incident was unacknowledged.
    | Resolve
      -- ^ The incident was resolved.
    | Escalate
      -- ^ The incident was escalated.
    | Assign
      -- ^ The incident was assigned to a user.
    | Annotate
      -- ^ A note was added to the incident.
    | ReachTriggerLimit
      -- ^ The incident has reached the log entry trigger limit and
      -- will not create any more.
    | RepeatEscalationPath
      -- ^ The incident has reached the end of its escalation policy
      -- and will restart.
    | ExhaustEscalationPath
      -- ^ The incident has cycled through its escalation policy the
      -- max allowed number of times.
    | Notify
      -- ^ A user was notified.
      deriving (Eq, Show)

deriveNullary ''LogEntryType

-- | The raw log event.
data LogEntry = LogEntry'
    { _leId           :: LogEntryId
    , _leType         :: LogEntryType
    , _leCreatedAt    :: Date
    , _leNote         :: Maybe Text
    , _leAssignedUser :: Maybe User
    , _leChannel      :: Channel
    } deriving (Eq, Show)

instance FromJSON LogEntry where
    parseJSON = withObject "log_entry" $ \o -> do
        LogEntry' <$> o .:  "id"
                  <*> o .:  "type"
                  <*> o .:  "created_at"
                  <*> o .:? "note"
                  <*> o .:? "assigned_user"
                  <*> parseJSON (Object o)

-- | Id of the log entry.
makeLens "_leId" ''LogEntry

-- | Time at which the log entry was created.
makeLens "_leCreatedAt" ''LogEntry

-- | The type of the log entry.
makeLens "_leType" ''LogEntry

-- | Optional field containing an action note, if one was included
-- with the action.
makeLens "_leNote" ''LogEntry

-- | Only for assign, escalate log entries. The user to which the incident is
-- assigned.
makeLens "_leAssignedUser" ''LogEntry

-- | Representation of the means by which the action was channeled and the
-- possible perfomer of this action.
makeLens "_leChannel" ''LogEntry

data NotificationType
    = SMS
    | Email
    | Phone
    | IosPushNotification
      deriving (Eq, Show)

deriveNullary ''NotificationType

data NotificationStatus
    = Success
    | Blocked
    | Busy
    | Failed
    | NoAnswer
    | BadAddress
      deriving (Eq, Show)

deriveNullary ''NotificationStatus

data Notification = Notification
    { _nType    :: !NotificationType
    , _nStatus  :: !NotificationStatus
    , _nAddress :: Address
    } deriving (Eq, Show)

deriveJSON ''Notification

-- | Type of notification.
makeLens "_nType" ''Notification

-- | The current status of the notification.
makeLens "_nStatus" ''Notification

-- | The address to which the notification was sent. I.e., an email address,
-- phone number, or iPhone name.
makeLens "_nAddress" ''Notification

-- | Notify log entries correspond to notifications sent to users. They have a
-- distinct format from action log entries.
data NotifyEntry = NotifyEntry'
    { _neCreatedAt'    :: Date
    , _neUser'         :: User
    , _neNotification' :: Notification
    } deriving (Eq, Show)

deriveRecord ''NotifyEntry

-- | Time at which the log entry was created
neCreatedAt :: Lens' NotifyEntry UTCTime
neCreatedAt = neCreatedAt'._D

-- | User who was notified
neUser :: Lens' NotifyEntry User
neUser = neUser'

-- | Object representing the notification itself
neNotification :: Lens' NotifyEntry Notification
neNotification = neNotification'

data Entry
    = LogEntry LogEntry
      -- ^ Log entries come in a wide variety of types. Most types use this format,
      -- the exception being the 'NotifyEntry' type.
    | NotifyEntry NotifyEntry
      -- ^ A user was notified.
      deriving (Eq, Show)

makePrisms ''Entry

data ListLogs = ListLogs
    { _llTimeZone'   :: TZ
    , _llSince'      :: Maybe Date
    , _llUntil'      :: Maybe Date
    , _llIsOverview' :: Bool'
    } deriving (Eq, Show)

instance Paginate ListLogs

queryRequest ''ListLogs

-- | Time zone in which dates in the result will be rendered.
--
-- _Default:_ UTC.
llTimeZone :: Lens' (Request ListLogs s b) TimeZone
llTimeZone = upd.llTimeZone'._TZ

-- | The start of the date range over which you want to search.
llSince :: Lens' (Request ListLogs s b) (Maybe UTCTime)
llSince = upd.llSince'.mapping _D

-- | The end of the date range over which you want to search.
llUntil :: Lens' (Request ListLogs s b) (Maybe UTCTime)
llUntil = upd.llUntil'.mapping _D

-- | If true, will only return log entries of type trigger, acknowlldge, or resolve.
--
-- _Default:_ false.
llIsOverview :: Lens' (Request ListLogs s b) Bool
llIsOverview = upd.llIsOverview'._B

-- | List all incident log entries across the entire account.
--
-- @GET \/log_entries@
--
-- _See:_ <http://developer.pagerduty.com/documentation/rest/log_entries/list>
listLogs :: Request ListLogs s [LogEntry]
listLogs =
    mk ListLogs
        { _llTimeZone'   = def
        , _llSince'      = Nothing
        , _llUntil'      = Nothing
        , _llIsOverview' = F
        } & path   .~ logs
          & query  .~ includes
          & unwrap .~ key "log_entries"

-- | List all incident log entries that describe interactions with a specific user.
--
-- @GET \/users\/\:user_id\/log_entries@
--
-- _See:_ <http://developer.pagerduty.com/documentation/rest/log_entries/user_log_entries>
listUserLogs :: UserId -> Request ListLogs s [LogEntry]
listUserLogs u = listLogs & path .~ "users" % u % logs

-- | List all incident log entries for a specific incident.
--
-- @GET \/incidents\/\:incident_id\/log_entries@
--
-- _See:_ <http://developer.pagerduty.com/documentation/rest/log_entries/incident_log_entries>
listIncidentLogs :: IncidentKey -> Request ListLogs s [LogEntry]
listIncidentLogs i = listLogs & path .~ "incidents" % i % logs

newtype GetLog = GetLog
    { _glTimeZone' :: TZ
    } deriving (Eq, Show)

queryRequest ''GetLog

-- | Time zone in which dates in the result will be rendered.
--
-- _Default:_ UTC.
glTimeZone :: Lens' (Request GetLog s b) TimeZone
glTimeZone = upd.glTimeZone'._TZ

-- | Get details for a specific incident log entry. This method provides additional
-- information you can use to get at raw event data.
--
-- @GET \/log_entries\/\:id@
--
-- _See:_ <http://developer.pagerduty.com/documentation/rest/log_entries/show>
getLog :: LogEntryId -> Request GetLog s LogEntry
getLog l =
    mk GetLog
        { _glTimeZone' = def
        } & path  .~ logs % l
          & query .~ includes
