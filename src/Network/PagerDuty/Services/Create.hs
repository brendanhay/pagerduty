{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Module      : Network.PagerDuty.Services.Create
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new service.
--
-- @POST \/services@
--
-- See: <http://developer.pagerduty.com/documentation/rest/services/create>
module Network.PagerDuty.Services.Create
    ( createService
    , csName
    , csEscalationPolicyId
    , csType
    , csVendorId
    , csDescription
    , csAcknowledgementTimeout
    , csAutoResolveTimeout
    , csSeverityFilter
    ) where

import           Data.Aeson.Lens
import qualified Data.ByteString.Char8            as BS
import           Data.Text                        (Text)
import           Network.HTTP.Types
import           Network.PagerDuty.Services.Types
import           Network.PagerDuty.TH
import           Network.PagerDuty.Types

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
