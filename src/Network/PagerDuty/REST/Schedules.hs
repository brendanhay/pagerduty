{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

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
    -- * List Schedules
      ListSchedules
    , listSchedules
    , lsQuery

    -- * Get Schedule
    , GetSchedule
    , getSchedule
    , gsSince
    , gsUntil
    , gsTimeZone

    -- * Get Schedule Users
    , GetScheduleUsers
    , getScheduleUsers
    , gsuSince
    , gsuUntil

    -- * Get Schedule Entries
    , GetScheduleEntries
    , getScheduleEntries
    , gseSince
    , gseUntil
    , gseOverflow
    , gseTimeZone
    , gseUserId

    -- * Create Schedule
    , CreateSchedule
    , createSchedule
    , csName
    , csOverflow
    , csTimeZone
    , csScheduleLayers

    -- * Update Schedule
    , UpdateSchedule
    , updateSchedule
    , usOverflow
    , usTimeZone
    , usScheduleLayers

    -- * Preview Schedule
    , PreviewSchedule
    , previewSchedule
    , psSince
    , psUntil
    , psOverflow
    , psName
    , psTimeZone
    , psScheduleLayers

    -- * Delete Schedule
    , deleteSchedule

    -- * Types
    , RestrictionType (..)
    , Restriction
    , rStartTimeOfDay
    , rDurationSeconds

    , Rotation
    , rMemberOrder
    , rUser

    , ScheduleLayer
    , slName
    , slRenderedScheduleEntries
    , slRestrictionType
    , slRestrictions
    , slPriority
    , slStart
    , slEnd
    , slRenderedCoveragePercentage
    , slRotationTurnLengthSeconds
    , slRotationVirtualStart
    , slUsers

    , HasScheduleInfo (..)
    , ScheduleInfo
    , Schedule
    , sScheduleLayers
    , sOverridesSubschedule
    , sFinalSchedule
    ) where

import Control.Lens                 hiding ((.=))
import Data.Aeson
import Data.ByteString.Builder      (Builder)
import Data.Text                    (Text)
import Data.Time
import Network.HTTP.Types.QueryLike
import Network.PagerDuty.REST.Users (User)
import Network.PagerDuty.TH
import Network.PagerDuty.Types

default (Builder)

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
    lens _rStartTimeOfDay (\r x -> r { _rStartTimeOfDay = x }) . _D

makeLens "_rDurationSeconds" ''Restriction

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
    { _siId'                 :: ScheduleId
    , _siName'               :: Text
    , _siTimeZone'           :: TZ
    , _siToday'              :: Date
    , _siEscalationPolicies' :: [PolicyId]
    } deriving (Eq, Show)

deriveRecord ''ScheduleInfo

class HasScheduleInfo a where
    scheduleInfo        :: Lens' a ScheduleInfo

    -- | The unique identifier of the schedule.
    sId                 :: Lens' a ScheduleId
    -- | The name of the schedule.
    sName               :: Lens' a Text
    -- | The time zone of the schedule.
    sTimeZone           :: Lens' a TimeZone
    -- | The current day in the schedule's time zone.
    sToday              :: Lens' a UTCTime
    -- | An array of all the escalation policies that uses this schedule.
    sEscalationPolicies :: Lens' a [PolicyId]

    sId                 = scheduleInfo.siId'
    sName               = scheduleInfo.siName'
    sTimeZone           = scheduleInfo.siTimeZone'._TZ
    sToday              = scheduleInfo.siToday'._D
    sEscalationPolicies = scheduleInfo.siEscalationPolicies'

instance HasScheduleInfo ScheduleInfo where
    scheduleInfo = id

data Schedule = Schedule
    { _sInfo                 :: ScheduleInfo
    , _sScheduleLayers       :: [ScheduleLayer]
    , _sOverridesSubschedule :: ScheduleLayer
    , _sFinalSchedule        :: ScheduleLayer
    } deriving (Eq, Show)

deriveJSON ''Schedule

instance HasScheduleInfo Schedule where
    scheduleInfo = lens _sInfo (\s x -> s { _sInfo = x })

-- | A list of schedule layers. See the schedule layers parameters for details.
makeLens "_sScheduleLayers" ''Schedule

-- | The schedule layer object where all the overrides are stored.
makeLens "_sOverridesSubschedule" ''Schedule

-- | The final schedule layer object.
makeLens "_sFinalSchedule" ''Schedule

newtype ListSchedules = ListSchedules
    { _lsQuery' :: Maybe Text
    } deriving (Eq, Show)

queryRequest ''ListSchedules

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

queryRequest ''GetSchedule

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

queryRequest ''GetScheduleUsers

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

queryRequest ''GetScheduleEntries

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
gseTimeZone :: Lens' (Request GetScheduleEntries s b) (Maybe TimeZone)
gseTimeZone = upd.gseTimeZone'.mapping _TZ

-- | To filter the returned on-call schedule entries by a specific user, you
-- can optionally add the user_id parameter to the query.
gseUserId :: Lens' (Request GetScheduleEntries s b) (Maybe UserId)
gseUserId = upd.gseUserId'

-- | List schedule entries that are active for a given time range for a
-- specified on-call schedule.
--
-- @GET \/schedules\/\:id\/entries@
--
-- _See:_ <http://developer.pagerduty.com/documentation/rest/schedules/entries>
getScheduleEntries :: UserId
                   -> Date -- ^ 'gseSince'
                   -> Date -- ^ 'gseUntil'
                   -> Request GetScheduleEntries s b
getScheduleEntries i s u =
    mk GetScheduleEntries
        { _gseSince'    = s
        , _gseUntil'    = u
        , _gseOverflow' = F
        , _gseTimeZone' = Nothing
        , _gseUserId'   = Nothing
        } & path .~ schedules % i

data CreateSchedule = CreateSchedule
    { _csName'           :: Text
    , _csOverflow'       :: !Bool'
    , _csTimeZone'       :: TZ
    , _csScheduleLayers' :: [ScheduleLayer]
    } deriving (Eq, Show)

makeLenses ''CreateSchedule

instance ToJSON CreateSchedule where
    toJSON cs = object
        [ "name"     .= _csName' cs
        , "overflow" .= _csOverflow' cs
        , "schedule" .= object
            [ "time_zone"       .= _csTimeZone' cs
            , "schedule_layers" .= _csScheduleLayers' cs
            ]
        ]

instance QueryLike CreateSchedule where
    toQuery = const []

-- | Any on-call schedule entries that pass the date range bounds will be
-- truncated at the bounds, unless 'csOverflow' is set to 'True'.
--
-- Default: 'False'.
csOverflow :: Lens' (Request CreateSchedule s b) Bool
csOverflow = upd.csOverflow'._B

-- | The name of the schedule.
csName :: Lens' (Request CreateSchedule s b) Text
csName = upd.csName'

-- | The time zone of the schedule.
--
-- Default: 'UTC'.
csTimeZone :: Lens' (Request CreateSchedule s b) TimeZone
csTimeZone = upd.csTimeZone'._TZ

-- | A list of schedule layers.
csScheduleLayers :: Lens' (Request CreateSchedule s b) [ScheduleLayer]
csScheduleLayers = upd.csScheduleLayers'

-- | Create a new on-call schedule.
--
-- @POST \/schedules@
--
-- _See:_ <http://developer.pagerduty.com/documentation/rest/schedules/create>
createSchedule :: Text            -- ^ 'csName'
               -> TimeZone        -- ^ 'csTimeZone'
               -> [ScheduleLayer] -- ^ 'csScheduleLayers'
               -> Request CreateSchedule s Schedule
createSchedule n z ls =
    mk CreateSchedule
        { _csName'           = n
        , _csOverflow'       = F
        , _csTimeZone'       = TZ z
        , _csScheduleLayers' = ls
        } & path .~ schedules

data UpdateSchedule = UpdateSchedule
    { _usOverflow'       :: !Bool'
    , _usTimeZone'       :: TZ
    , _usScheduleLayers' :: [ScheduleLayer]
    } deriving (Eq, Show)

makeLenses ''UpdateSchedule

instance ToJSON UpdateSchedule where
    toJSON us = object
        [ "overflow" .= _usOverflow' us
        , "schedule" .= object
            [ "time_zone"       .= _usTimeZone' us
            , "schedule_layers" .= _usScheduleLayers' us
            ]
        ]

instance QueryLike UpdateSchedule where
    toQuery = const []

-- | Any on-call schedule entries that pass the date range bounds will be
-- truncated at the bounds, unless 'usOverflow' is set to 'True'.
--
-- Default: 'False'.
usOverflow :: Lens' (Request UpdateSchedule s b) Bool
usOverflow = upd.usOverflow'._B

-- | The time zone of the schedule.
usTimeZone :: Lens' (Request UpdateSchedule s b) TimeZone
usTimeZone = upd.usTimeZone'._TZ

-- | A list of schedule layers.
usScheduleLayers :: Lens' (Request UpdateSchedule s b) [ScheduleLayer]
usScheduleLayers = upd.usScheduleLayers'

-- | Update an existing on-call schedule.
--
-- _Note:_ You cannot delete schedule layers. You must include all layers in
-- your update request. To delete a layer, set the end parameter to schedule its
-- termination.
--
-- @PUT \/schedules\/\:id@
--
-- _See:_ <http://developer.pagerduty.com/documentation/rest/schedules/update>
updateSchedule :: ScheduleId
               -> TimeZone        -- ^ 'usTimeZone'
               -> [ScheduleLayer] -- ^ 'usScheduleLayers'
               -> Request UpdateSchedule s Schedule
updateSchedule s z ls =
    mk UpdateSchedule
        { _usOverflow'       = F
        , _usTimeZone'       = TZ z
        , _usScheduleLayers' = ls
        } & path .~ schedules % s

data PreviewSchedule = PreviewSchedule
    { _psSince'          :: Maybe Date
    , _psUntil'          :: Maybe Date
    , _psName'           :: Text
    , _psOverflow'       :: !Bool'
    , _psTimeZone'       :: TZ
    , _psScheduleLayers' :: [ScheduleLayer]
    } deriving (Eq, Show)

makeLenses ''PreviewSchedule

instance ToJSON PreviewSchedule where
    toJSON ps = object
        [ "since"    .= _psSince' ps
        , "until"    .= _psUntil' ps
        , "name"     .= _psName' ps
        , "overflow" .= _psOverflow' ps
        , "schedule" .= object
            [ "time_zone"       .= _psTimeZone' ps
            , "schedule_layers" .= _psScheduleLayers' ps
            ]
        ]

instance QueryLike PreviewSchedule where
    toQuery = const []

-- | The start of the date range over which you want to return on-call schedule
-- entries and on-call schedule layers.
psSince :: Lens' (Request PreviewSchedule s b) (Maybe UTCTime)
psSince = upd.psSince'.mapping _D

-- | The end of the date range over which you want to return schedule entries
-- and on-call schedule layers.
psUntil :: Lens' (Request PreviewSchedule s b) (Maybe UTCTime)
psUntil = upd.psUntil'.mapping _D

-- | Any on-call schedule entries that pass the date range bounds will be
-- truncated at the bounds, unless 'psOverflow' is set to 'True'.
--
-- Default: 'False'.
psOverflow :: Lens' (Request PreviewSchedule s b) Bool
psOverflow = upd.psOverflow'._B

-- | The name of the schedule.
psName :: Lens' (Request PreviewSchedule s b) Text
psName = upd.psName'

-- | The time zone of the schedule.
--
-- Default: 'UTC'.
psTimeZone :: Lens' (Request PreviewSchedule s b) TimeZone
psTimeZone = upd.psTimeZone'._TZ

-- | A list of schedule layers.
psScheduleLayers :: Lens' (Request PreviewSchedule s b) [ScheduleLayer]
psScheduleLayers = upd.psScheduleLayers'

-- | Preview what a schedule would look like without saving it. This work the
-- same as the update or create actions, except that the result is not
-- persisted. Preview optionally takes two additional arguments, since and until,
-- deliminating the span of the preview.
--
-- @POST \/schedules\/preview@
--
-- _See:_ <http://developer.pagerduty.com/documentation/rest/schedules/preview>
previewSchedule :: Text            -- ^ 'psName'
                -> TimeZone        -- ^ 'psTimeZone'
                -> [ScheduleLayer] -- ^ 'psScheduleLayers'
                -> Request PreviewSchedule s Schedule
previewSchedule n z ls =
    mk PreviewSchedule
        { _psSince'          = Nothing
        , _psUntil'          = Nothing
        , _psName'           = n
        , _psOverflow'       = F
        , _psTimeZone'       = TZ z
        , _psScheduleLayers' = ls
        } & path .~ schedules % "preview"

-- | Delete an on-call schedule.
--
-- @DELETE \/schedules\/\:id@
--
-- _See:_ <http://developer.pagerduty.com/documentation/rest/schedules/delete>
deleteSchedule :: ScheduleId -> Request Empty s Empty
deleteSchedule s = empty & path .~ schedules % s
