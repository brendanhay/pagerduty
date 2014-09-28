{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Module      : Network.PagerDuty.Services
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
-- have its 'svcHtmlUrl attribute set to 'Nothing'.
--
-- See: <http://developer.pagerduty.com/documentation/rest/services>
module Network.PagerDuty.Services
    (
    -- * Operations
    -- ** List
      listServices
    , lsTimeZone

    -- ** Create
    , createService
    , csName
    , csEscalationPolicyId
    , csType
    , csVendorId
    , csDescription
    , csAcknowledgementTimeout
    , csAutoResolveTimeout
    , csSeverityFilter

    -- ** Get

    -- ** Update

    -- ** Delete

    -- ** Disable

    -- ** Enable

    -- ** Regenerate Key

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

import           Control.Lens               hiding ((.=))
import           Data.Aeson                 hiding (Error)
import           Data.Aeson.Lens
import qualified Data.ByteString.Char8      as BS
import           Data.ByteString.Conversion
import           Data.Text                  (Text)
import           Network.HTTP.Types
import           Network.PagerDuty.TH
import           Network.PagerDuty.Types

req :: (ToJSON a, ToByteString p)
    => StdMethod
    -> p
    -> Unwrap
    -> a
    -> Request a s r
req m p u = req' m ("services", p) u

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

deriveJSON ''IncidentCounts
makeLenses ''IncidentCounts

data EmailIncidentCreation
    = OnNewEmail
      -- ^ Open a new incident for each trigger email.
    | OnNewEmailSubject
      -- ^ Open a new incident for each new trigger email subject.
    | OnlyIfNoOpenIncidents
      -- ^ Open a new incident only if an open incident does not already exist.
      deriving (Eq, Show)

deriveJSON ''EmailIncidentCreation

data EmailFilterMode
    = AllEmail
      -- ^ Accept all incoming email
    | OrRulesEmail
      -- ^ Accept email only if it matches ONE OR MORE rules below
    | AndRulesEmail
      -- ^ Accept email only if it matches ALL of the rules below
      deriving (Eq, Show)

deriveJSONWith hyphenated ''EmailFilterMode

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

deriveJSON ''ServiceStatus

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

deriveJSON ''ServiceType

data PolicyInfo = PolicyInfo
    { _pinfoId   :: PolicyId
    , _pinfoName :: Text
    } deriving (Eq, Show)

makeLenses ''PolicyInfo
deriveJSON ''PolicyInfo

data MatchMode
    = Always
    | Match
    | NoMatch
      deriving (Eq, Show)

deriveJSONWith hyphenated ''MatchMode

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

deriveJSON ''EmailFilters

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

deriveJSONWith (unprefixed 3 underscored) ''SeverityFilter

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
makeLens "_svcCreatedAt" ''Service

-- | The current state of the Service.
makeLens "_svcStatus" ''Service

-- | The date/time when the most recent incident was created for this service.
makeLens "_svcLastIncidentTimestamp" ''Service

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

deriveJSON ''Service

newtype ListServices = ListServices
    { _lsTimeZone :: Maybe Text
    } deriving (Eq, Show)

-- | List existing services.
--
-- @GET \/services@
--
-- See: <http://developer.pagerduty.com/documentation/rest/services/list>
listServices :: Request ListServices Token [Service]
listServices = req GET BS.empty (key "escalation_policies")
    (ListServices
        { _lsTimeZone = Nothing
        }) & rqQuery <>~ includes

-- | Time zone in which dates in the result will be rendered.
-- Defaults to account default time zone.
makeLens "_lsTimeZone" ''ListServices

deriveJSON ''ListServices

instance Paginate ListServices

data CreateService = CreateService
    { _csName                   :: Text
    , _csEscalationPolicyId     :: PolicyId
    , _csType                   :: !ServiceType
    , _csVendorId               :: Maybe VendorId
    , _csDescription            :: Maybe Text
    , _csAcknowledgementTimeout :: Maybe Int
    , _csAutoResolveTimeout     :: Maybe Int
    , _csSeverityFilter         :: Maybe SeverityFilter
    } deriving (Eq, Show)

-- | Creates a new service.
--
-- @POST \/services@
--
-- See: <http://developer.pagerduty.com/documentation/rest/services/create>
createService :: Text
              -> PolicyId
              -> ServiceType
              -> Request CreateService Token Service
createService n p t = req GET BS.empty (key "service")
    CreateService
        { _csName                   = n
        , _csEscalationPolicyId     = p
        , _csType                   = t
        , _csVendorId               = Nothing
        , _csDescription            = Nothing
        , _csAcknowledgementTimeout = Nothing
        , _csAutoResolveTimeout     = Nothing
        , _csSeverityFilter         = Nothing
        }

-- | The name of the service.
makeLens "_csName" ''CreateService

-- | The id of the escalation policy to be used by this service.
makeLens "_csEscalationPolicyId" ''CreateService

-- | The type of service to create.
makeLens "_csType" ''CreateService

-- | PagerDuty's internal vendor identifier for this service. For more information
-- about a specific vendor, please contact PagerDuty Support.
makeLens "_csVendorId" ''CreateService

-- | A description for your service. 1024 character maximum.
makeLens "_csDescription" ''CreateService

-- | The duration in seconds before an incidents acknowledged in this service
-- become triggered again.
--
-- Defaults to 30 minutes.
makeLens "_csAcknowledgementTimeout" ''CreateService

-- | The duration in seconds before a triggered incident auto-resolves itself.
--
-- Defaults to 4 hours.
makeLens "_csAutoResolveTimeout" ''CreateService

-- | Specifies what severity levels will create a new open incident.
makeLens "_csSeverityFilter" ''CreateService

deriveJSON ''CreateService
