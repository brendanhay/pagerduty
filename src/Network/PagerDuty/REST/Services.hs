{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- Module      : Network.PagerDuty.REST.Services
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This API lets you access and manipulate the services across your account.
--
-- A service is an endpoint (like Nagios, email, or an API call) that
-- generates events, which Pagerduty normalizes and dedupes, creating incidents.
--
-- When a service is shown inlined in other resources, a deleted service will
-- have its @html_url@ attribute set to 'Nothing'.
--
-- See: <http://developer.pagerduty.com/documentation/rest/services>
module Network.PagerDuty.REST.Services
    (
    -- * List Services
      ListServices
    , listServices
    , lsTimeZone

    -- * Create Service
    , CreateService
    , createService
    , csName
    , csDescription
    , csType
    , csVendorId
    , csEscalationPolicyId
    , csAcknowledgementTimeout
    , csAutoResolveTimeout
    , csSeverityFilter

    -- * Get Service
    , getService

    -- * Update Service
    , UpdateService
    , updateService
    , usName
    , usEscalationPolicyId
    , usDescription
    , usAcknowledgementTimeout
    , usAutoResolveTimeout
    , usSeverityFilter

    -- * Delete Service
    , deleteService

    -- * Enable Service
    , enableService

    -- * Disable Service
    , disableService

    -- * Regenerate Key
    , regenerateKey

    -- * Types
    , EmailFilterMode       (..)
    , EmailIncidentCreation (..)
    , MatchMode             (..)
    , ServiceStatus         (..)
    , ServiceType           (..)
    , SeverityFilter        (..)

    , IncidentCounts
    , cntTriggered
    , cntAcknowledged
    , cntResolved
    , cntTotal

    , EmailFilters          (..)
    , efsId
    , efsSubjectMode
    , efsSubjectRegex
    , efsBodyMode
    , efsBodyRegex
    , efsFromEmailMode
    , efsFromEmailRegex

    , PolicyInfo
    , pinfoId
    , pinfoName

    , Service
    , svcId
    , svcName
    , svcDescription
    , svcServiceUrl
    , svcServiceKey
    , svcAutoResolveTimeout
    , svcAcknowledgementTimeout
    , svcCreatedAt
    , svcStatus
    , svcLastIncidentTimestamp
    , svcEmailIncidentCreation
    , svcIncidentCounts
    , svcEmailFilterMode
    , svcType
    , svcEscalationPolicy
    , svcEmailFilters
    , svcSeverityFilter
    ) where

import Control.Lens
import Data.Aeson.Lens
import Data.ByteString.Builder (Builder)
import Data.Text               (Text)
import Data.Time
import Network.HTTP.Types
import Network.PagerDuty.TH
import Network.PagerDuty.Types

default (Builder)

services :: Path
services = "services"

includes :: Query
includes =
    [ ("include[]", Just "escalation_policy")
    , ("include[]", Just "email_filters")
    ]

data IncidentCounts = IncidentCounts
    { _cntTriggered    :: !Int
    , _cntAcknowledged :: !Int
    , _cntResolved     :: !Int
    , _cntTotal        :: !Int
    } deriving (Eq, Show)

deriveRecord ''IncidentCounts

data EmailIncidentCreation
    = OnNewEmail
      -- ^ Open a new incident for each trigger email.
    | OnNewEmailSubject
      -- ^ Open a new incident for each new trigger email subject.
    | OnlyIfNoOpenIncidents
      -- ^ Open a new incident only if an open incident does not already exist.
      deriving (Eq, Show)

deriveNullary ''EmailIncidentCreation

data EmailFilterMode
    = AllEmail
      -- ^ Accept all incoming email
    | OrRulesEmail
      -- ^ Accept email only if it matches ONE OR MORE rules below
    | AndRulesEmail
      -- ^ Accept email only if it matches ALL of the rules below
      deriving (Eq, Show)

deriveNullaryWith hyphenated ''EmailFilterMode

data ServiceStatus
    = Active
      -- ^ The service is enabled and has no open incidents.
    | Warning
      -- ^ The service is enabled and has one or more acknowledged incidents.
    | Critical
      -- ^ The service is enabled and has one or more triggered incidents.
    | Maintenance
      -- ^ The service is under maintenance, no new incidents will be
      -- triggered during maintenance mode.
    | Disabled
      -- ^ The service is disabled and will not have any new triggered incidents.
      deriving (Eq, Show)

deriveNullary ''ServiceStatus

data ServiceType
    = CloudKick
    | GenericEmail
    | GenericEventsApi
    | Keynote
    | Nagios
    | Pingdom
    | ServerDensity
    | SqlMonitor
      deriving (Eq, Show)

deriveNullary ''ServiceType

data PolicyInfo = PolicyInfo
    { _pinfoId   :: PolicyId
    , _pinfoName :: Text
    } deriving (Eq, Show)

deriveRecord ''PolicyInfo

data MatchMode
    = Always
    | Match
    | NoMatch
      deriving (Eq, Show)

deriveNullaryWith hyphenated ''MatchMode

-- FIXME: Tighten up this type! Make the regex required for match/no-match.
data EmailFilters = EmailFilters
    { _efsId             :: EmailFilterId
    , _efsSubjectMode    :: Maybe MatchMode
    , _efsSubjectRegex   :: Maybe Text
    , _efsBodyMode       :: Maybe MatchMode
    , _efsBodyRegex      :: Maybe Text
    , _efsFromEmailMode  :: Maybe MatchMode
    , _efsFromEmailRegex :: Maybe Text
    } deriving (Eq, Show)

deriveJSON ''EmailFilters

-- | The email filter ID.
makeLens "_efsId" ''EmailFilters

-- | One of always, match, no-match, which, respectively, means to not filter
-- the email trigger by subject, filter it if the email subject matches the
-- given regex, or filter if it doesn't match the given regex.
--
-- Defaults to always.
makeLens "_efsSubjectMode" ''EmailFilters

-- | The regex to be used when subject_mode is match or no-match.
-- It is a required parameter on such cases.
makeLens "_efsSubjectRegex" ''EmailFilters

-- | One of always, match, no-match, which, respectively, means to not filter
-- the email trigger by body, filter it if the body email matches the given regex,
-- or filter if it doesn't match the given regex.
--
-- Defaults to always.
makeLens "_efsBodyMode" ''EmailFilters

-- | The regex to be used when body_mode is match or no-match.
-- It is a required parameter on such cases.
makeLens "_efsBodyRegex" ''EmailFilters

-- | One of always, match, no-match, which, respectively, means to not filter
-- the email trigger by its from address, filter it if the email from address
-- matches the given regex, or filter if it doesn't match the given regex.
--
-- Defaults to always.
makeLens "_efsFromEmailMode" ''EmailFilters

-- | The regex to be used when from_email_mode is match or no-match.
-- It is a required parameter on such cases.
makeLens "_efsFromEmailRegex" ''EmailFilters

data SeverityFilter
    = SevCritical
      -- ^ Incidents are created when an alarm enters the Critical state.
    | SevCriticalOrWarning
      -- ^ Incidents are created when an alarm enters the Critical OR Warning states
    | SevOnAny
      -- ^ SQL Monitor: Incidents are created for alerts of any severity.
    | SevOnHigh
      -- ^ SQL Monitor: Incidents are created for alerts with high severity.
    | SevOnMediumHigh
      -- ^ SQL Monitor: Incidents are created for with high or medium severity
      deriving (Eq, Show)

deriveNullaryWith (dropped 3 underscored) ''SeverityFilter

data Service = Service
    { _svcId                     :: ServiceId
    , _svcName                   :: Text
    , _svcDescription            :: Text
    , _svcServiceUrl             :: Text
    , _svcServiceKey             :: ServiceKey
    , _svcAutoResolveTimeout     :: Maybe Int
    , _svcAcknowledgementTimeout :: Maybe Int
    , _svcCreatedAt              :: Date
    , _svcStatus                 :: ServiceStatus
    , _svcLastIncidentTimestamp  :: Maybe Date
    , _svcEmailIncidentCreation  :: Maybe EmailIncidentCreation
    , _svcIncidentCounts         :: IncidentCounts
    , _svcEmailFilterMode        :: EmailFilterMode
    , _svcType                   :: ServiceType
    , _svcEscalationPolicy       :: Maybe PolicyInfo   -- FIXME: extract from inline.
    , _svcEmailFilters           :: Maybe EmailFilters -- FIXME: extract from inline.
    , _svcSeverityFilter         :: Maybe SeverityFilter
    } deriving (Eq, Show)

deriveJSON ''Service

-- | A unique identifier for this service.
makeLens "_svcId" ''Service

-- | The name of the service.
makeLens "_svcName" ''Service

-- | The description of the service
makeLens "_svcDescription" ''Service

-- | Relative URL that corresponds to this service.
makeLens "_svcServiceUrl" ''Service

-- | For Email services, the serviceKey is the associated email address
-- for the service.
-- For all other service types, this is the unique key used for API calls.
makeLens "_svcServiceKey" ''Service

-- | Time in seconds that an incident changes to the Triggered State after
-- being Acknowledged. Value is 'Nothing' if the feature is disabled.
makeLens "_svcAutoResolveTimeout" ''Service

-- | Time in seconds that an incident is automatically resolved if left open for
-- that long. Value is 'Nothing' is the feature is disabled.
makeLens "_svcAcknowledgementTimeout" ''Service

-- | The date/time when this service was created.
svcCreatedAt :: Lens' Service UTCTime
svcCreatedAt = lens _svcCreatedAt (\s x -> s { _svcCreatedAt = x }) . _Date

-- | The current state of the Service.
makeLens "_svcStatus" ''Service

-- | The date/time when the most recent incident was created for this service.
svcLastIncidentTimestamp :: Lens' Service (Maybe UTCTime)
svcLastIncidentTimestamp =
    lens _svcLastIncidentTimestamp
         (\s x -> s { _svcLastIncidentTimestamp = x }) . mapping _Date

-- | If the service is a Generic Email service, this describes what kind of
-- emails create an incident.
makeLens "_svcEmailIncidentCreation" ''Service

-- | An object with the number of incidents corresponding to these states.
makeLens "_svcIncidentCounts" ''Service

-- | If the service is a Generic Email service, this describes which types of
-- email will generate an incident.
makeLens "_svcEmailFilterMode" ''Service

-- | The service type.
makeLens "_svcType" ''Service

-- | An object containing the ID and name of the escalation policy used by this
-- service.
makeLens "_svcEscalationPolicy" ''Service

-- | An object containing inline Email Filters.
-- Note that only genericEmail services have filters.
makeLens "_svcEmailFilters" ''Service

-- | Specifies what severity levels will create a new open incident.
makeLens "_svcSeverityFilter" ''Service

newtype ListServices = ListServices
    { _lsTimeZone' :: Maybe TZ
    } deriving (Eq, Show)

instance Paginate ListServices

deriveQuery ''ListServices

-- | Time zone in which dates in the result will be rendered.
--
-- Defaults to account default time zone.
lsTimeZone :: Lens' (Request ListServices s r) (Maybe TimeZone)
lsTimeZone = upd.lsTimeZone'.mapping _TZ

-- | List existing services.
--
-- @GET \/services@
--
-- See: <http://developer.pagerduty.com/documentation/rest/services/list>
listServices :: Request ListServices s [Service]
listServices =
    mk ListServices
        { _lsTimeZone' = Nothing
        } & path   .~ services
          & query  .~ includes
          & unwrap .~ key "escalation_policies"

data CreateService = CreateService
    { _csName'                   :: Text
    , _csEscalationPolicyId'     :: PolicyId
    , _csType'                   :: !ServiceType
    , _csVendorId'               :: Maybe VendorId
    , _csDescription'            :: Maybe Text
    , _csAcknowledgementTimeout' :: Maybe Int
    , _csAutoResolveTimeout'     :: Maybe Int
    , _csSeverityFilter'         :: Maybe SeverityFilter
    } deriving (Eq, Show)

deriveBody ''CreateService

-- | The name of the service.
csName :: Lens' (Request CreateService s r) Text
csName = upd.csName'

-- | A description for your service. 1024 character maximum.
csDescription :: Lens' (Request CreateService s r) (Maybe Text)
csDescription = upd.csDescription'

-- | The type of service to create.
csType :: Lens' (Request CreateService s r) ServiceType
csType = upd.csType'

-- | PagerDuty's internal vendor identifier for this service. For more information
-- about a specific vendor, please contact PagerDuty Support.
csVendorId :: Lens' (Request CreateService s r) (Maybe VendorId)
csVendorId = upd.csVendorId'

-- | The id of the escalation policy to be used by this service.
csEscalationPolicyId :: Lens' (Request CreateService s r) PolicyId
csEscalationPolicyId = upd.csEscalationPolicyId'

-- | The duration in seconds before an incidents acknowledged in this service
-- become triggered again.
--
-- Defaults to 30 minutes.
csAcknowledgementTimeout :: Lens' (Request CreateService s r) (Maybe Int)
csAcknowledgementTimeout = upd.csAcknowledgementTimeout'

-- | The duration in seconds before a triggered incident auto-resolves itself.
--
-- Defaults to 4 hours.
csAutoResolveTimeout :: Lens' (Request CreateService s r) (Maybe Int)
csAutoResolveTimeout = upd.csAutoResolveTimeout'

-- | Specifies what severity levels will create a new open incident.
csSeverityFilter :: Lens' (Request CreateService s r) (Maybe SeverityFilter)
csSeverityFilter = upd.csSeverityFilter'

-- | Creates a new service.
--
-- @POST \/services@
--
-- See: <http://developer.pagerduty.com/documentation/rest/services/create>
createService :: Text
              -> PolicyId
              -> ServiceType
              -> Request CreateService s Service
createService n p t =
    mk CreateService
        { _csName'                   = n
        , _csEscalationPolicyId'     = p
        , _csType'                   = t
        , _csVendorId'               = Nothing
        , _csDescription'            = Nothing
        , _csAcknowledgementTimeout' = Nothing
        , _csAutoResolveTimeout'     = Nothing
        , _csSeverityFilter'         = Nothing
        } & path   .~ services
          & unwrap .~ key "service"

-- | Get details about an existing service.
--
-- @GET services\/\:id@
--
-- See: <http://developer.pagerduty.com/documentation/rest/services/show>
getService :: ServiceId -> Request Empty s Service
getService i = empty
    & path   .~ services % i
    & query <>~ includes
    & unwrap .~ key "service"

data UpdateService = UpdateService
    { _usName'                   :: Maybe Text
    , _usDescription'            :: Maybe Text
    , _usEscalationPolicyId'     :: Maybe PolicyId
    , _usAcknowledgementTimeout' :: Maybe Int
    , _usAutoResolveTimeout'     :: Maybe Int
    , _usSeverityFilter'         :: Maybe SeverityFilter
    } deriving (Eq, Show)

deriveBody ''UpdateService

-- | The name of the service.
usName :: Lens' (Request UpdateService s r) (Maybe Text)
usName = upd.usName'

-- | A description for your service. 1024 character maximum.
usDescription :: Lens' (Request UpdateService s r) (Maybe Text)
usDescription = upd.usDescription'

-- | The id of the escalation policy to be used by this service.
usEscalationPolicyId :: Lens' (Request UpdateService s r) (Maybe PolicyId)
usEscalationPolicyId = upd.usEscalationPolicyId'

-- | The duration in seconds before an incidents acknowledged in this service
-- become triggered again.
--
-- Defaults to 30 minutes.
usAcknowledgementTimeout :: Lens' (Request UpdateService s r) (Maybe Int)
usAcknowledgementTimeout = upd.usAcknowledgementTimeout'

-- | The duration in seconds before a triggered incident auto-resolves itself.
--
-- Defaults to 4 hours.
usAutoResolveTimeout :: Lens' (Request UpdateService s r) (Maybe Int)
usAutoResolveTimeout = upd.usAutoResolveTimeout'

-- | Specifies what severity levels will create a new open incident.
usSeverityFilter :: Lens' (Request UpdateService s r) (Maybe SeverityFilter)
usSeverityFilter = upd.usSeverityFilter'

-- | Update an existing service.
--
-- @PUT services\/\:id@
--
-- See: <http://developer.pagerduty.com/documentation/rest/services/update>
updateService :: ServiceId -> Request UpdateService s Service
updateService i =
    mk UpdateService
        { _usName'                   = Nothing
        , _usEscalationPolicyId'     = Nothing
        , _usDescription'            = Nothing
        , _usAcknowledgementTimeout' = Nothing
        , _usAutoResolveTimeout'     = Nothing
        , _usSeverityFilter'         = Nothing
        } & meth   .~ PUT
          & path   .~ services % i
          & unwrap .~ key "service"

-- | Delete an existing service. Once the service is deleted, it will not be
-- accessible from the web UI and new incidents won't be able to be created
-- for this service.
--
-- @DELETE \/services/:id@
--
-- See: <http://developer.pagerduty.com/documentation/rest/services/delete>
deleteService :: ServiceId -> Request Empty s Empty
deleteService i = empty & meth .~ DELETE & path .~ services % i

-- | Enable a previously disabled service.
--
-- @PUT services\/\:id\/enable@
--
-- See: <http://developer.pagerduty.com/documentation/rest/services/enable>
enableService :: RequesterId -> ServiceId -> Request Empty s Empty
enableService r i = auth (enableServiceBasic i) & query .~ [("requester_id", r)]

-- | A version of 'enableService' which uses HTTP Basic authentication and
-- doesn't require a 'RequesterId'.
enableServiceBasic :: ServiceId -> Request Empty Basic Empty
enableServiceBasic i = empty & meth .~ PUT & path .~ services % i % "enable"

-- | Disable a service. Once a service is disabled, it will not be able to
-- create incidents until it is enabled again.
--
-- @PUT services\/\:id\/disable@
--
-- See: <http://developer.pagerduty.com/documentation/rest/services/disable>
disableService :: RequesterId -> ServiceId -> Request Empty s Empty
disableService r i = auth (disableServiceBasic i) & query .~ [("requester_id", r)]

-- | A version of 'disableService' which uses HTTP Basic authentication and
-- doesn't require a 'RequesterId'.
disableServiceBasic :: ServiceId -> Request Empty Basic Empty
disableServiceBasic i = empty & meth .~ PUT & path .~ services % i % "disable"

-- | Regenerate a new service key for an existing service.
--
-- _Warning! The service's previous key will be invalidated, and existing
-- monitoring integrations will need to be modified to use the new key!_
--
-- @POST services\/\:id\/regenerate_key@
--
-- See: <http://developer.pagerduty.com/documentation/rest/services/regenerate_key>
regenerateKey :: ServiceId -> Request Empty s Service
regenerateKey i = empty & meth .~ POST & path .~ services % i % "regenerate_key"
