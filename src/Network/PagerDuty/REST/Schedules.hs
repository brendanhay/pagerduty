{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Module      : Network.PagerDuty.REST.Schedules
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | On call schedules determine the time periods that users are on-call. Only when
-- a user is on-call he is eligible to receive alerts from incidents.
--
-- This API allows users to manipulate on-call schedules.
module Network.PagerDuty.REST.Schedules
    (

    ) where

schedules :: Path
schedules = "schedules"

data RestrictionType
    = Daily
    | Weekly
      deriving (Eq, Show)

deriveNullary ''RestrictionType

data Restriction = Restriction
    { _rStartTimeOfDay  :: Date
    , _rDurationSeconds :: !Int
    } deriving (Eq, Show)

deriveJSON ''Restriction

rStartTimeOfDay :: Lens' Restriction UTCTime
rStartTimeOfDay =
    lens _rStartTimeOfDay (\r x -> r { _rStartTimeOfDay = x }) . _Date

rDurationSeconds :: Lens' Restriction UTCTime
rDurationSeconds =
    lens _rDurationSeconds (\r x -> r { _rDurationSeconds = x }) . _Date

data Rotation = Rotation
    { _rMemberOrder :: !Int
    , _rUser        :: User
    } deriving (Eq, Show)

deriveRecord ''Rotation

-- | A schedule is composed of multiple schedule layers.
--
-- A layer is composed of a group of people who will rotate through the same
-- shift. In a basic weekly schedule, you create a single layer where each member
-- is on-call for one week, with a set day and time for transferring on-call duty.
--
-- When a schedule has multiple layers, the layer can be ordered using the
-- priority field. The layer with the highest priority has precedence over the
-- layers with lower priority. You can use restrictions to control how layers
-- overlap.
--
-- The override layer is a special layer where all the override entries are
-- stored.
--
-- The final layer is a special layer that contains the result of all the previous
-- layers put together. This layer cannot be edited.
data ScheduleLayer = ScheduleLayer
    { _slName                       :: Text
    , _slRenderedScheduleEntries    :: [ScheduleId] -- FIXME: what is this a list of?
    , _slRestrictionType            :: !RestrictionType
    , _slRestrictions               :: [Restriction]
    , _slPriority                   :: !Int
    , _slStart                      :: Date
    , _slEnd                        :: Date
    , _slRenderedCoveragePercentage :: !Double
    , _slRotationTurnLengthSeconds  :: !Int
    , _slRotationVirtualStart       :: Date
    , _slUsers                      :: [Rotation]
    } deriving (Eq, Show)

-- FIXME: most of these fields are maybes

deriveJSON ''ScheduleLayer

makeLens "_slName" ''ScheduleLayer

-- | This is a list of entries to be rendered for the current time range.
makeLens "_slRenderedScheduleEntries" ''ScheduleLayer

-- | This specifies the type of restrictions present on this layer.
makeLens "_slRestrictionType" ''ScheduleLayer

-- | A list of time restrictions for this layer.
makeLens "_slRestrictions" ''ScheduleLayer

-- | The priority of the layer. Layers with higher priority will override
-- layers with a lower priority.
makeLens "_slPriority" ''ScheduleLayer

-- | The effective start date of the layer
makeLens "_slStart" ''ScheduleLayer

-- | The effective end date of the layer. If 'Nothing', the layer has no end date.
makeLens "_slEnd" ''ScheduleLayer

-- | The percentage of the time range covered by this layer.
makeLens "_slRenderedCoveragePercentage" ''ScheduleLayer

-- | The duration of each on-call shift in seconds.
makeLens "_slRotationTurnLengthSeconds" ''ScheduleLayer

-- | The effective start time of the layer. This can be before the start
-- time of the schedule.
makeLens "_slRotationVirtualStart" ''ScheduleLayer

-- | An ordered list of users in this layer. The member_order field controls
--  the order in which the users apear in the rotation.
makeLens "_slUsers" ''ScheduleLayer

data ScheduleInfo = ScheduleInfo
    { _sId                 :: ScheduleId
    , _sName               :: Text
    , _sTimeZone           :: TZ
    , _sToday              :: Date
    , _sEscalationPolicies :: [PolicyId]
    } deriving (Eq, Show)

makeClassy ''ScheduleInfo

data Schedule = Schedule
    { _sInfo'                 :: ScheduleInfo
    , _sScheduleLayers'       :: [ScheduleLayer]
    , _sOverridesSubschedule' :: ScheduleLayer
    , _sFinalSchedule'        :: ScheduleLayer
    } deriving (Eq, Show)

deriveRecord ''Schedule

instance HasScheduleInfo Schedule where
    scheduleInfo = sInfo'

newtype ListSchedules = ListSchedules
    { _lsQuery' :: Maybe Text
    } deriving (Eq, Show)

deriveQuery ''ListSchedules

-- | Filters the result, showing only the schedules whose name matches the query.
lsQuery :: Lens' (Request ListSchedules s b) (Maybe Text)
lsQuery = upd.lsQuery'

instance Paginate ListSchedules

-- | List existing on-call schedules.
--
-- @GET \/schedules@
--
-- _See:_ <http://developer.pagerduty.com/documentation/rest/schedules/list>
listSchedules :: RequesterId -> Request ListSchedules s [ScheduleInfo]
listSchedules r = auth listSchedulesBasic & query .~ [("requester_id", r)]

-- | A version of 'listSchedules' which uses HTTP Basic authentication and
-- doesn't require a 'RequesterId'.
listSchedulesBasic :: Request ListSchedules s [ScheduleInfo]
listSchedulesBasic =
    mk ListSchedules
        { _lsQuery' = Nothing
        } & path .~ schedules

data GetSchedule = GetSchedule
    { _gsSince'    :: Maybe Date
    , _gsUntil'    :: Maybe Date
    , _gsTimeZone' :: Maybe TZ
    } deriving (Eq, Show)

deriveQuery ''GetSchedule

-- | The start of the date range over which you want to return on-call schedule
-- entries and on-call schedule layers.
gsSince :: Lens' (Request GetSchedule s b) (Maybe UTCTime)
gsSince = upd.gsSince'.mapping _D

-- | The end of the date range over which you want to return schedule entries
-- and on-call schedule layers.
gsUntil :: Lens' (Request GetSchedule s b) (Maybe UTCTime)
gsUntil = upd.gsUntil'.mapping _D

-- | Time zone in which dates in the result will be rendered.
--
-- _Default:_ account time zone.
gsTimeZone :: Lens' (Request GetSchedule s b) (Maybe TimeZone)
gsTimeZone = upd.gsTimeZone'.mapping _TZ

-- | Show detailed information about a schedule, including entries for each
-- layer and sub-schedule.
--
-- @GET \/schedules\/\:id@
--
-- _See:_ <http://developer.pagerduty.com/documentation/rest/schedules/show>
getSchedule :: Request GetSchedule s Schedule
getSchedule =
    mk GetSchedule
        { _gsSince'    = Nothing
        , _gsUntil'    = Nothing
        , _gsTimeZone' = Nothing
        } & path .~ schedules


data GetScheduleUsers = GetScheduleUsers
    { _gsuSince' :: Maybe Date
    , _gsuUntil' :: Maybe Date
    } deriving (Eq, Show)

deriveQuery ''GetScheduleUsers

-- | The start of the date range over which you want to return on-call users.
gsuSince :: Lens' (Request GetScheduleUsers s b) (Maybe UTCTime)
gsuSince = upd.gsuSince'.mapping _D

-- | The end time of the date range over which you want to return on-call users.
gsuUntil :: Lens' (Request GetScheduleUsers s b) (Maybe UTCTime)
gsuUntil = upd.gsuUntil'.mapping _D

-- | List all the users on-call in a given schedule for a given time range.
--
-- @GET \/schedules\/\:id\/users@
--
-- _See:_ <http://developer.pagerduty.com/documentation/rest/schedules/users>
getScheduleUsers :: UserId -> Request GetScheduleUsers s [User]
getScheduleUsers u =
    mk GetScheduleUsers
        { _gsuSince' = Nothing
        , _gsuUntil' = Nothing
        } & path .~ schedules % u % "users"

data GetScheduleEntries = GetScheduleEntries
    { _gseSince'    :: Date
    , _gseUntil'    :: Date
    , _gseOverflow' :: !Bool'
    , _gseTimeZone' :: Maybe TZ
    , _gseUserId'   :: Maybe UserId
    } deriving (Eq, Show)

deriveRecord ''GetScheduleEntries

-- | The start of the date range over which you want to return on-call
-- schedule entries.
-- The maximum range queryable at once is three months.
gseSince :: Lens' (Request GetScheduleEntries s b) UTCTime
gseSince = upd.gseSince'._D

-- | The end of the date range over which you want to return schedule entries.
gseUntil :: Lens' (Request GetScheduleEntries s b) UTCTime
gseUntil = upd.gseUntil'._D

-- | Any on-call schedule entries that pass the date range bounds will be
-- truncated at the bounds, unless the parameter overflow=true is passed.
--
-- Default: false.
--
-- Example: if your schedule is a rotation that changes daily at midnight UTC,
-- and your date range is from 2011-06-01T10:00:00Z to 2011-06-01T14:00:00Z: If
-- you don't pass the overflow=true parameter, you will get one schedule entry
-- returned with a start of 2011-06-01T10:00:00Z and end of 2011-06-01T14:00:00Z.
-- If you do pass the overflow=true parameter, you will get one schedule entry
-- returned with a start of 2011-06-01T00:00:00Z and end of 2011-06-02T00:00:00Z.
gseOverflow :: Lens' (Request GetScheduleEntries s b) Bool
gseOverflow = upd.gseOverflow'._B

-- | Time zone in which dates in the result will be rendered.
--
-- Default: account time zone.
gseTimeZone :: Lens' (Request GetScheduleEntries s b) TimeZone
gseTimeZone = upd.gseTimeZone'.mapping _TZ

-- | To filter the returned on-call schedule entries by a specific user, you
-- can optionally add the user_id parameter to the query.
gseUserId :: Lens' (Request GetScheduleEntries s b) (Maybe Text)
gseUserId = upd.gseUserId'

-- | List schedule entries that are active for a given time range for a
-- specified on-call schedule.
--
-- @GET \/schedules\/\:id\/entries@
--
-- _See:_ <http://developer.pagerduty.com/documentation/rest/schedules/entries>
getScheduleEntries =
    mk Get

-- | Create a new on-call schedule.
--
-- @POST \/schedules@
--
-- _See:_ <http://developer.pagerduty.com/documentation/rest/schedules/create>
createSchedule = undefined

-- | Update an existing on-call schedule.
--
-- @PUT \/schedules\/\:id@
--
-- _See:_ <http://developer.pagerduty.com/documentation/rest/schedules/update>
updateSchedule = undefined

-- | Preview the configuration of an on-call schedule.
--
-- @POST \/schedules\/preview@
--
-- _See:_ <http://developer.pagerduty.com/documentation/rest/schedules/preview>
previewSchedule = undefined

-- | Delete an on-call schedule.
--
-- @DELETE \/schedules\/\:id@
--
-- _See:_ <http://developer.pagerduty.com/documentation/rest/schedules/delete>
deleteSchedule = undefined
