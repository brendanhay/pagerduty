{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- Module      : Network.PagerDuty.REST.Incidents
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | PagerDuty receives updates (called events) from monitoring systems through
-- services (like Nagios, email or generic API calls). Upon receiving an event,
-- active services will create a new incident and begin escalating it as
-- defined by the service's escalation policy. Events can be de-duplicated into
-- existing incidents based on service de-duplication rules to prevent you from
-- being overwhelmed by event storms.
--
-- An incident can be open, acknowledged or resolved. Whenever an incident is
-- created, it is assigned to a user (according to the escalation process, as
-- described by escalation rules and schedules). The assigned user has a chance
-- to either acknowledge that he is working on it, or to resolve it.
--
-- Resolving an incident closes it, whereas acknowledging it halts the
-- escalation process. If the incident is not resolved by the service's
-- incident ack timeout it continues up the escalation chain.
--
-- When an incident is triggered or when it is escalated it creates alerts
-- (also known as notifications). Alerts are messages containing the details of
-- the incident, and can be sent through SMS, email and phone calls.
--
-- Incidents and Incident Counts from the last 30 days are returned by
-- default. To change this default date range, see the documentation for the
-- since, until or dateRange parameters.
--
-- Note: that to create an incident you need to trigger it through the
-- "Network.PagerDuty.Integration" API.
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/incidents>
module Network.PagerDuty.REST.Incidents
    (
    -- * List Incidents
      ListIncidents
    , listIncidents
    , lsSince
    , lsUntil
    , lsDateRange
    , lsStatus
    , lsIncidentKey
    , lsService
    , lsAssignedToUser
    , lsTimeZone
    , lsSortBy

    -- * Count Incidents
    , CountIncidents
    , countIncidents
    , cSince
    , cUntil
    , cDateRange
    , cStatus
    , cIncidentKey
    , cService
    , cAssignedToUser

    -- * Get Incident
    , getIncident

    -- * Update Incidents
    , UpdateIncidents
    , updateIncidents
    , updateIncidentsBasic
    , uiIncidents

    -- * Resolve Incident
    , resolveIncident
    , resolveIncidentBasic

    -- * Acknowledge Incident
    , acknowledgeIncident
    , acknowledgeIncidentBasic

    -- * Reassign Incident
    , ReassignIncident
    , reassignIncident
    , riEscalationPolicy
    , riEscalationLevel
    , riAssignedToUser

    -- * Types
    , Sort           (..)
    , _Desc
    , _Asc

    , Field          (..)

    , IncidentStatus (..)
    , UpdateStatus   (..)

    , UpdateIncident
    , iiId
    , iiStatus
    , iiEscalationLevel
    , iiEscalationPolicy
    , iiAssignedToUser

    , UpdatedIncidents
    , uiId
    , uiStatus
    , uiIncidentNumber
    , uiUrl
    , uiError

    , Assignee
    , aAt
    , aObject

    , HasIncident (..)
    , Incident
    ) where

import           Control.Applicative              hiding (empty)
import           Control.Lens
import           Data.Aeson
import           Data.ByteString.Conversion       (ToByteString(..), toByteString')
import           Data.Default.Class
import           Data.Maybe
import           Data.Monoid                      hiding (All)
import           Data.Text                        (Text)
import qualified Data.Text.Encoding               as Text
import           Data.Time
import           Network.HTTP.Types
import           Network.PagerDuty.Internal.Query
import           Network.PagerDuty.Internal.TH
import           Network.PagerDuty.Internal.Types

default (Path)

incidents :: Path
incidents = "incidents"

data Sort  a
    = Desc a
    | Asc  a
      deriving (Eq, Show)

makePrisms ''Sort

data Field
    = IncidentNumber
      -- ^ The number of your incident.
    | CreatedOn
      -- ^ The date/time the incident was triggered.
    | ResolvedOn
      -- ^ The date/time the incident was resolved.
      deriving (Eq, Show)

instance QueryValues (Sort Field) where
    queryValues s = case s of
        Desc f -> ["desc:" <> field f]
        Asc  f -> ["asc:"  <> field f]
      where
        field IncidentNumber = "incidentNumber"
        field CreatedOn      = "createdOn"
        field ResolvedOn     = "resolvedOn"

data Range = All
    deriving (Eq, Show)

instance QueryValues Range where
    queryValues All = ["all"]

data IncidentStatus
    = ITriggered
    | IAcknowledged
    | IResolved
    | IOther Text
      deriving (Eq, Show)

instance ToByteString IncidentStatus where
    builder = \case
        ITriggered    -> "triggered"
        IAcknowledged -> "acknowledged"
        IResolved     -> "resolved"
        IOther t      -> builder t

instance FromJSON IncidentStatus where
    parseJSON = withText "status" $ pure . \case
        "triggered"    -> ITriggered
        "acknowledged" -> IAcknowledged
        "resolved"     -> IResolved
        t              -> IOther t

instance ToJSON IncidentStatus where
    toJSON = String . Text.decodeUtf8 . toByteString'

data Assignee = Assignee
    { _aAt     :: Date
    , _aObject :: Object
    } deriving (Eq, Show)

deriveRecord ''Assignee

data Incident = Incident
    { _iIncidentNumber'        :: !Int
    , _iStatus'                :: !IncidentStatus
    , _iCreatedOn'             :: Date
    , _iHtmlUrl'               :: Text
    , _iIncidentKey'           :: Maybe IncidentKey
    , _iService'               :: Object
    , _iEscalationPolicy'      :: Maybe Object
    , _iAssignedTo'            :: [Assignee]
    , _iAcknowledgers'         :: [Assignee]
    , _iLastStatusChangeBy'    :: Maybe Assignee
    , _iLastStatusChangeOn'    :: Maybe Date
    , _iTriggerSummaryData'    :: Object
    , _iTriggerDetailsHtmlUrl' :: Text
    } deriving (Eq, Show)

deriveRecord ''Incident

class HasIncident a where
    incident               :: Lens' a Incident

    -- | The number of the incident. This is unique across your account.
    iIncidentNumber        :: Lens' a Int

    -- | The current status of the incident. Valid statuses are:
    iStatus                :: Lens' a IncidentStatus

    -- | The date/time the incident was triggered.
    iCreatedOn             :: Lens' a UTCTime

    -- | The PagerDuty website URL where the incident can be viewed and further
    -- actions taken. This is not the resource URL.
    iHtmlUrl               :: Lens' a Text

    -- | The incident's de-duplication key. See the PagerDuty Integration API
    -- docs for further details.
    iIncidentKey           :: Lens' a (Maybe IncidentKey)

    -- | The PagerDuty service that the incident belongs to. The service will
    -- contain fields of its own.
    iService               :: Lens' a Object

    -- | The escalation policy that the incident belongs to. The policy will
    -- contain fields of its own.
    iEscalationPolicy      :: Lens' a (Maybe Object)

    -- | The list of assignments of the incident. An assignment is an object
    -- containing the assigned user as well as the date/time the incident was
    -- assigned to that user. The user will contain fields of its own.
    -- This list is empty if the status of the incident is resolved.
    iAssignedTo            :: Lens' a [Assignee]

    -- | The list of acknowledgements of the incident. An acknowledgement is an
    -- object containing the acknowleding object (either a user or the
    -- integration API) as well as the date/time the incident was acknowledged.
    -- This field is only present if the status of the incident is acknowledged.
    -- This field is sorted in ascending order by acknowledgement time.
    iAcknowledgers         :: Lens' a [Assignee]

    -- | The user who is responsible for the incident's last status change. If
    -- the incident is in the acknowledged or resolved status, this will be the user
    -- that took the first acknowledged or resolved action. If the incident was
    -- automatically resolved (say through the Event Integration API), or if the
    -- incident is in the triggered state, this will be null. User fields are the
    -- same as in the assignedToUser field above.
    iLastStatusChangeBy    :: Lens' a (Maybe Assignee)

    -- | The date/time the incident's status last changed.
    iLastStatusChangeOn    :: Lens' a (Maybe UTCTime)

    -- | Some condensed information regarding the initial event that triggered
    -- this incident. This data will be a set of key/value pairs that vary depending
    -- on what sort of event triggered the incident (email, Event API request,
    -- etc). For instance, if an email triggered the incident, then the
    -- triggerSummaryData will likely contain a subject. There are no guarantees
    -- o nthe full presence of this data for every incident.
    iTriggerSummaryData    :: Lens' a Object

    -- | The PagerDuty website URL where the full details regarding the initial
    -- event that triggered this incident can be found. (This is not the resource
    -- URL.)
    iTriggerDetailsHtmlUrl :: Lens' a Text

    iIncidentNumber        = incident.iIncidentNumber'
    iStatus                = incident.iStatus'
    iCreatedOn             = incident.iCreatedOn'._D
    iHtmlUrl               = incident.iHtmlUrl'
    iIncidentKey           = incident.iIncidentKey'
    iService               = incident.iService'
    iEscalationPolicy      = incident.iEscalationPolicy'
    iAssignedTo            = incident.iAssignedTo'
    iAcknowledgers         = incident.iAcknowledgers'
    iLastStatusChangeBy    = incident.iLastStatusChangeBy'
    iLastStatusChangeOn    = incident.iLastStatusChangeOn'.mapping _D
    iTriggerSummaryData    = incident.iTriggerSummaryData'
    iTriggerDetailsHtmlUrl = incident.iTriggerDetailsHtmlUrl'

instance HasIncident Incident where
    incident = id

data ListIncidents = ListIncidents
    { _lsSince'          :: Maybe Date
    , _lsUntil'          :: Maybe Date
    , _lsDateRange'      :: Maybe Range
    , _lsStatus'         :: Maybe (CSV IncidentStatus)
    , _lsIncidentKey'    :: Maybe IncidentKey
    , _lsService'        :: Maybe (CSV ServiceId)
    , _lsAssignedToUser' :: Maybe (CSV UserId)
    , _lsTimeZone'       :: !TZ
    , _lsSortBy'         :: Maybe (Sort Field)
    } deriving (Eq, Show)

queryRequest ''ListIncidents

instance Paginate ListIncidents

-- | The start of the date range over which you want to search.
lsSince :: Lens' (Request ListIncidents s b) (Maybe UTCTime)
lsSince = upd.lsSince'.mapping _D

-- | The end of the date range over which you want to search.
--
-- /Note: If you leave off either since or until, a 30 day default range is
-- applied to your open ended range. Not including the since parameter will set
-- the date range to until - 30 days. Likewise, if you leave off until,
-- it is set to since + 30 days.
--
-- Defaults to the last 30 days if you leave off both. The size of the date
-- range must be less than 180 days.
lsUntil :: Lens' (Request ListIncidents s b) (Maybe UTCTime)
lsUntil = upd.lsUntil'.mapping _D

-- | When set, the since and until parameters and defaults are
-- ignored. Use this to get all incidents since the account was created.
lsDateRange :: Lens' (Request ListIncidents s b) Bool
lsDateRange = upd.lens get_ (flip set_)
  where
    get_   = (Just All ==) . view lsDateRange'
    set_ x = lsDateRange' .~ listToMaybe [All | x]

-- | Returns only the incidents currently in the passed status(es).
lsStatus :: Lens' (Request ListIncidents s b) (Maybe [IncidentStatus])
lsStatus = upd.lsStatus'.mapping _CSV

-- | Returns only the incidents with the passed de-duplication key.
--
-- /See:/ "Network.PagerDuty.Integration"
lsIncidentKey :: Lens' (Request ListIncidents s b) (Maybe IncidentKey)
lsIncidentKey = upd.lsIncidentKey'

-- | Returns only the incidents associated with the passed service(s). This
-- expects one or more service IDs.
lsService :: Lens' (Request ListIncidents s b) (Maybe [ServiceId])
lsService = upd.lsService'.mapping _CSV

-- | Returns only the incidents currently assigned to the passed user(s). This
-- expects one or more user IDs.
--
-- /Note:/ When using the assignedToUser filter, you will only receive
-- incidents with statuses of triggered or acknowledged. This is because resolved
-- incidents are not assigned to any user.
lsAssignedToUser :: Lens' (Request ListIncidents s b) (Maybe [UserId])
lsAssignedToUser = upd.lsAssignedToUser'.mapping _CSV

-- | Time zone in which dates in the result will be rendered.
--
-- /Default:/ 'UTC'.
lsTimeZone :: Lens' (Request ListIncidents s b) TimeZone
lsTimeZone = upd.lsTimeZone'._TZ

-- | Used to specify both the field you wish to sort the results on, as well as
-- the direction (ascending/descending) of the results.
lsSortBy :: Lens' (Request ListIncidents s b) (Maybe (Sort Field))
lsSortBy = upd.lsSortBy'

-- | The PagerDuty incidents query API can be used to query current and
-- historical PagerDuty incidents over a date range, letting you build custom
-- dashboards or incident reports. The API allows for searching for incidents
-- with multiple filters or query parameters, various sorts, and also supports
-- the pagination of results.
--
-- @GET \/incidents@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/incidents/list>
listIncidents :: Request ListIncidents s [Incident]
listIncidents =
    mk ListIncidents
        { _lsSince'          = Nothing
        , _lsUntil'          = Nothing
        , _lsDateRange'      = Nothing
        , _lsStatus'         = Nothing
        , _lsIncidentKey'    = Nothing
        , _lsService'        = Nothing
        , _lsAssignedToUser' = Nothing
        , _lsTimeZone'       = def
        , _lsSortBy'         = Nothing
        } & path .~ incidents

-- | Get detailed information about an incident. Accepts either an incident id,
-- or an incident number.
--
-- @GET \/incidents\/\:id@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/incidents/show>
getIncident :: IncidentId -> Request Empty s Incident
getIncident i = empty & path .~ incidents % i

data CountIncidents = CountIncidents
    { _cSince'          :: Maybe Date
    , _cUntil'          :: Maybe Date
    , _cDateRange'      :: Maybe Range
    , _cStatus'         :: Maybe (CSV IncidentStatus)
    , _cIncidentKey'    :: Maybe IncidentKey
    , _cService'        :: Maybe (CSV ServiceId)
    , _cAssignedToUser' :: Maybe (CSV UserId)
    } deriving (Eq, Show)

queryRequest ''CountIncidents

-- | The start of the date range over which you want to search.
cSince :: Lens' (Request CountIncidents s b) (Maybe UTCTime)
cSince = upd.cSince'.mapping _D

-- | The end of the date range over which you want to search.
--
-- /Note:/ If you leave off either since or until, a 30 day default range is
-- applied to your open ended range. Not including the since parameter will set
-- the date range to until @-30@ days. Likewise, if you leave off until,
-- it is set to since @+30@ days.
--
-- /Default:/ The last 30 days if you leave off both. The size of the date
-- range must be less than 180 days.
cUntil :: Lens' (Request CountIncidents s b) (Maybe UTCTime)
cUntil = upd.cUntil'.mapping _D

-- | When set, the since and until parameters and defaults are
-- ignored. Use this to get all counts since the account was created.
cDateRange :: Lens' (Request CountIncidents s b) Bool
cDateRange = upd.lens get_ (flip set_)
  where
    get_   = (Just All ==) . view cDateRange'
    set_ x = cDateRange' .~ listToMaybe [All | x]

-- | Only counts the incidents currently in the passed status(es).
cStatus :: Lens' (Request CountIncidents s b) (Maybe [IncidentStatus])
cStatus = upd.cStatus'.mapping _CSV

-- | Returns only the incidents with the passed de-duplication key.
--
-- /See:/ "Network.PagerDuty.Integration"
cIncidentKey :: Lens' (Request CountIncidents s b) (Maybe IncidentKey)
cIncidentKey = upd.cIncidentKey'

-- | Only counts the incidents associated with the passed service(s).
-- This is expecting one or more service IDs.
cService :: Lens' (Request CountIncidents s b) (Maybe [ServiceId]) -- list1
cService = upd.cService'.mapping _CSV

-- | Only counts the incidents currently assigned to the passed user(s).
-- This is expecting one or more user IDs.
--
-- /Note:/ When using the assigned_to_user filter, you will only count
-- incidents with statuses of triggered or acknowledged. This is because resolved
-- incidents are not assigned to any user.
cAssignedToUser :: Lens' (Request CountIncidents s b) (Maybe [UserId]) -- list1
cAssignedToUser = upd.cAssignedToUser'.mapping _CSV

newtype Count = Count Int
    deriving (Eq, Ord, Show, Bounded, Enum, Num, Integral, Real)

instance FromJSON Count where
    parseJSON = withObject "count" $ \o -> Count <$> o .: "total"

-- | Use this query if you are simply looking for the count of incidents that
-- match a given query. This should be used if you don't need access to the
-- actual incident details.
--
-- @GET \/incidents\/count@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/incidents/count>
countIncidents :: Request CountIncidents s Count
countIncidents =
    mk CountIncidents
        { _cSince'          = Nothing
        , _cUntil'          = Nothing
        , _cDateRange'      = Nothing
        , _cStatus'         = Nothing
        , _cIncidentKey'    = Nothing
        , _cService'        = Nothing
        , _cAssignedToUser' = Nothing
        } & path .~ incidents % "count"


data UpdateStatus
    = UAcknowledged
    | UResolved
      deriving (Eq, Show)

deriveNullary ''UpdateStatus

data UpdateIncident = UpdateIncident
    { _iiId               :: IncidentId
    , _iiStatus           :: Maybe UpdateStatus
    , _iiEscalationLevel  :: Maybe Int
    , _iiEscalationPolicy :: Maybe EscalationPolicyId
    , _iiAssignedToUser   :: Maybe (CSV UserId)
    } deriving (Eq, Show)

deriveJSON ''UpdateIncident

-- | The id of the incident to update.
makeLens "_iiId" ''UpdateIncident

-- | The new status of the incident.
makeLens "_iiStatus" ''UpdateIncident

-- | Escalate incident to this level in the escalation policy.
makeLens "_iiEscalationLevel" ''UpdateIncident

-- | Delegate this incident to the specified escalation policy id.
-- This restarts the incident's escalation following the new policy.
makeLens "_iiEscalationPolicy" ''UpdateIncident

-- | List of user IDs to assign this incident to.
makeLens "_iiAssignedToUser" ''UpdateIncident

data UpdatedIncidents = UpdatedIncidents
    { _uiId             :: IncidentId
    , _uiStatus         :: IncidentStatus
    , _uiIncidentNumber :: !Int
    , _uiUrl            :: Text
    , _uiError          :: Maybe Object
    } deriving (Eq, Show)

deriveRecord ''UpdatedIncidents

data UpdateIncidents = UpdateIncidents
    { _uiIncidents' :: [UpdateIncident]
    } deriving (Eq, Show)

jsonRequest ''UpdateIncidents

uiIncidents :: Lens' (Request UpdateIncidents s b) [UpdateIncident]
uiIncidents = upd.uiIncidents'

-- | Acknowledge, resolve, escalate or reassign one or more incidents.
--
-- @PUT \/incidents@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/incidents/update>
updateIncidents :: RequesterId -> Request UpdateIncidents s [UpdatedIncidents]
updateIncidents r = auth updateIncidentsBasic & query .~ [("requester_id", r)]

-- | A version of 'updateIncidents' which uses HTTP Basic authentication and
-- doesn't require a 'RequesterId'.
updateIncidentsBasic :: Request UpdateIncidents Basic [UpdatedIncidents]
updateIncidentsBasic =
    mk UpdateIncidents
        { _uiIncidents' = []
        } & meth .~ PUT
          & path .~ incidents

-- | Resolve an incident.
--
-- @PUT \/incidents\/\:id\/resolve@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/incidents/resolve>
resolveIncident :: IncidentId
                -> RequesterId
                -> Request Empty s Empty
resolveIncident i r =
    auth (resolveIncidentBasic i) & query .~ [("requester_id", r)]

-- | A version of 'resolveIncident' which uses HTTP Basic authentication and
-- doesn't require a 'RequesterId'.
resolveIncidentBasic :: IncidentId -> Request Empty s Empty
resolveIncidentBasic i = empty & meth .~ PUT & path .~ incidents % i % "resolve"

-- | Acknowledge an incident.
--
-- @PUT \/incidents\/\:id\/acknowledge@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/incidents/acknowledge>
acknowledgeIncident :: IncidentId
                    -> RequesterId
                    -> Request Empty s Empty
acknowledgeIncident i r =
    auth (acknowledgeIncidentBasic i) & query .~ [("requester_id", r)]

-- | A version of 'acknowledgeIncident' which uses HTTP Basic authentication and
-- doesn't require a 'RequesterId'.
acknowledgeIncidentBasic :: IncidentId -> Request Empty s Empty
acknowledgeIncidentBasic i =
    empty & meth .~ PUT & path .~ incidents % i % "acknowledge"

data ReassignIncident = ReassignIncident
    { _riEscalationPolicy' :: Maybe EscalationPolicyId
    , _riEscalationLevel'  :: Maybe Int
    , _riAssignedToUser'   :: Maybe (CSV UserId)
    } deriving (Eq, Show)

jsonRequest ''ReassignIncident

-- | The ID of an escalation policy. Delegate the incident to this escalation
-- policy.
riEscalationPolicy :: Lens' (Request ReassignIncident s b) (Maybe EscalationPolicyId)
riEscalationPolicy = upd.riEscalationPolicy'

-- | Escalate incident to this level in the escalation policy.
riEscalationLevel :: Lens' (Request ReassignIncident s b) (Maybe Int)
riEscalationLevel = upd.riEscalationLevel'

-- | Comma separated list of user IDs to assign this incident to.
riAssignedToUser :: Lens' (Request ReassignIncident s b) (Maybe [UserId])
riAssignedToUser = upd.riAssignedToUser'.mapping _CSV

-- | Reassign an incident.
--
-- @PUT \/incidents\/\:id\/reassign@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/incidents/reassign>
reassignIncident :: IncidentId
                 -> RequesterId
                 -> Request ReassignIncident s Empty
reassignIncident i r =
    auth (reassignIncidentBasic i) & query .~ [("requester_id", r)]

-- | A version of 'reassignIncident' which uses HTTP Basic authentication and
-- doesn't require a 'RequesterId'.
reassignIncidentBasic :: IncidentId -> Request ReassignIncident s Empty
reassignIncidentBasic i =
    mk ReassignIncident
        { _riEscalationPolicy' = Nothing
        , _riEscalationLevel'  = Nothing
        , _riAssignedToUser'   = Nothing
        } & meth .~ PUT
          & path .~ incidents % i % "reassign"
