{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

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
-- have its @html_url@ attribute set to 'Nothing'.
--
-- See: <http://developer.pagerduty.com/documentation/rest/services>
module Network.PagerDuty.Services
    (
    -- * List Services
      listServices
    , lsTimeZone

    -- * Create Service
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
import Data.ByteString.Builder          (Builder)
import Data.Text                        (Text)
import Network.HTTP.Types
import Network.PagerDuty.Services.Types
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

newtype ListServices = ListServices
    { _lsTimeZone' :: Maybe TimeZone
    } deriving (Eq, Show)

instance Paginate ListServices

deriveJSON ''ListServices
makeLenses ''ListServices

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
          & query <>~ includes
          & unwrap .~ key "escalation_policies"

-- | Time zone in which dates in the result will be rendered.
--
-- Defaults to account default time zone.
lsTimeZone :: Lens' (Request ListServices s r) (Maybe TimeZone)
lsTimeZone = upd.lsTimeZone'

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

deriveJSON ''CreateService
makeLenses ''CreateService

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

deriveJSON ''UpdateService
makeLenses ''UpdateService

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
