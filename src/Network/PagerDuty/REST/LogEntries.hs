{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

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

    ) where

logs :: Path
logs = "log_entries"

includes :: Query
includes =
    [ ("include[]", Just "channel")
    , ("include[]", Just "incident")
    , ("include[]", Just "service")
    ]

data LogEntry = LogEntry
    deriving (Eq, Show)

data GetLogs
    { _leTimeZone'   :: Maybe TimeZone
    , _leSince'      :: Maybe Date
    , _leUntil'      :: Maybe Date
    , _leIsOverview' :: Bool
    } deriving (Eq, Show)

instance Paginated GetLogs

deriveJSON ''LogEntry
makeLenses ''LogEntry

-- | Time zone in which dates in the result will be rendered.
--
-- Defaults to UTC.
leTimeZone :: Lens' (Request LogEntry s b) TimeZone
leTimeZone = upd.leTimeZone'

-- | The start of the date range over which you want to search.
leSince :: Lens' (Request LogEntry s b) (Maybe Date)
leSince = upd.leSince'

-- | The end of the date range over which you want to search.
leUntil :: Lens' (Request LogEntry s b) (Maybe Date)
leUntil = upd.leUntil'

-- | If true, will only return log entries of type trigger, acknowledge, or resolve.
--
-- Defaults to false.
leIsOverview :: Lens' (Request LogEntry s b) Bool
leIsOverview = upd.leIsOverview'

-- | List all incident log entries across the entire account.
--
-- @GET \/log_entries@
--
-- See: <http://developer.pagerduty.com/documentation/rest/log_entries/list>
listLogs :: Request GetLogs s [LogEntry]
listLogs =
    mk GetLogs
        { _leTimeZone'   = def
        , _leSince'      = Nothing
        , _leUntil'      = Nothing
        , _leIsOverview' = False
        } & path   .~ logs
          & query  .~ includes
          & unwrap .~ key "log_entries"

-- | List all incident log entries that describe interactions with a specific user.
--
-- @GET \/users\/\:user_id\/log_entries@
--
-- See: <http://developer.pagerduty.com/documentation/rest/log_entries/user_log_entries>
listUserLogs :: UserId -> Request GetLogs s [LogEntry]
listUserLogs u = listLogs & path .~ "users" % u % logs

-- | List all incident log entries for a specific incident.
--
-- @GET \/incidents\/\:incident_id\/log_entries@
--
-- See: <http://developer.pagerduty.com/documentation/rest/log_entries/incident_log_entries>
listIncidentLogs :: Request GetLogs s [LogEntry]
listIncidentLogs i = listLogs & path .~ "incidents" % u % logs

newtype GetLogEntry = GetLogEntry
    { _gleTimeZone' :: TimeZone
    } deriving (Eq, Show)

deriveJSON ''GetLogEntry
makeLenses ''GetLogEntry

-- | Time zone in which dates in the result will be rendered.
--
-- Defaults to UTC.

-- | Get details for a specific incident log entry. This method provides additional
-- information you can use to get at raw event data.
--
-- @GET \/log_entries\/\:id@
--
-- See: <http://developer.pagerduty.com/documentation/rest/log_entries/show>
getLog :: LogEntryId -> Request GetLogEntry s LogEntry
getLog l =
    mk GetLogEntry
        { _gleTimeZone' = def
        } & path  .~ logs % l
          & query .~ includes
