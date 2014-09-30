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
    , csEscalationPolicyId
    , csType
    , csVendorId
    , csDescription
    , csAcknowledgementTimeout
    , csAutoResolveTimeout
    , csSeverityFilter

    -- * Get Service
    , getService

    -- * Update Service
    , updateService

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
import Data.Text                        (Text)
import Network.HTTP.Types
import Network.PagerDuty.Services.Types
import Network.PagerDuty.TH
import Network.PagerDuty.Types

services :: Setter (Request a s r) (Request a s r) Path [Path]
services = base "services"

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
listServices :: Request ListServices Token [Service]
listServices =
    mk ListServices
        { _lsTimeZone' = Nothing
        } & services .~ []
          & unwrap   .~ key "escalation_policies"
          & query   <>~ includes

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
              -> Request CreateService Token Service
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
        } & services .~ []
          & unwrap   .~ key "service"

-- | The name of the service.
csName :: Lens' (Request CreateService s r) Text
csName = upd.csName'

-- | The id of the escalation policy to be used by this service.
csEscalationPolicyId :: Lens' (Request CreateService s r) PolicyId
csEscalationPolicyId = upd.csEscalationPolicyId'

-- | The type of service to create.
csType :: Lens' (Request CreateService s r) ServiceType
csType = upd.csType'

-- | PagerDuty's internal vendor identifier for this service. For more information
-- about a specific vendor, please contact PagerDuty Support.
csVendorId :: Lens' (Request CreateService s r) (Maybe VendorId)
csVendorId = upd.csVendorId'

-- | A description for your service. 1024 character maximum.
csDescription :: Lens' (Request CreateService s r) (Maybe Text)
csDescription = upd.csDescription'

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
getService :: ServiceId -> Request Empty Token Service
getService i = empty
    & services .~ [P i]
    & unwrap   .~ key "service"
    & query   <>~ includes

data UpdateService = UpdateService
    { _usName'                   :: Text
    , _usEscalationPolicyId'     :: PolicyId
    , _usType'                   :: !ServiceType
    , _usDescription'            :: Maybe Text
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
updateService ServiceId -> Request UpdateService Token Service
updateService i =
    mk UpdateService
        { _usName'                   = n
        , _usEscalationPolicyId'     = p
        , _usType'                   = t
        , _usDescription'            = Nothing
        , _usAcknowledgementTimeout' = Nothing
        , _usAutoResolveTimeout'     = Nothing
        , _usSeverityFilter'         = Nothing
        } & services .~ [P i]
          & unwrap   .~ key "service"

-- | The name of the service.
usName :: Lens' (Request UpdateService s r) Text
usName = upd.usName'

-- | The id of the escalation policy to be used by this service.
usEscalationPolicyId :: Lens' (Request UpdateService s r) PolicyId
usEscalationPolicyId = upd.usEscalationPolicyId'

-- | PagerDuty's internal vendor identifier for this service. For more information
-- about a specific vendor, please contact PagerDuty Support.
usVendorId :: Lens' (Request UpdateService s r) (Maybe VendorId)
usVendorId = upd.usVendorId'

-- | A description for your service. 1024 character maximum.
usDescription :: Lens' (Request UpdateService s r) (Maybe Text)
usDescription = upd.usDescription'

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
deleteService :: ServiceId -> Request Empty Token Empty
deleteService i = empty & services .~ [P i] & meth .~ DELETE

-- | Enable a previously disabled service.
--
-- @PUT services\/\:id\/enable@
--
-- See: <http://developer.pagerduty.com/documentation/rest/services/enable>
enableService :: ServiceId -> Request Empty Token Empty
enableService i = empty & services .~ [P i, "enable"] & meth .~ PUT

-- | Disable a service. Once a service is disabled, it will not be able to
-- create incidents until it is enabled again.
--
-- @PUT services\/\:id\/disable@
--
-- See: <http://developer.pagerduty.com/documentation/rest/services/disable>
disableService :: ServiceId -> Request Empty Token Empty
disableService i = empty & services .~ [P i, "disable"] & meth .~ PUT

-- | Regenerate a new service key for an existing service.
--
-- _Warning! The service's previous key will be invalidated, and existing
-- monitoring integrations will need to be modified to use the new key!_
--
-- @POST services\/\:id\/regenerate_key@
--
-- See: <http://developer.pagerduty.com/documentation/rest/services/regenerate_key>
regenerateKey :: ServiceId -> Request Empty Token Service
regenerateKey i = empty & services .~ [P i, "regenerate_key"] & meth .~ POST
