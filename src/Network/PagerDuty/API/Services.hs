{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

-- Module      : Network.PagerDuty.API.Services
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.PagerDuty.API.Services
    ( ServiceDetails  (..)
    , CreateService   (..)
    , UpdateService   (..)
    , ServiceStatus   (..)
    , ServiceType     (..)
    , EmailFilterMode (..)
    , IncidentCounts  (..)
    , EmailIncidentCreation (..)

    , createService
    , updateService
    , listServices
    , getService
    , deleteService
    , disableService
    , disableService'
    , enableService
    , enableService'
    , regenerateServiceKey
    )
where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.ByteString            (ByteString)
import           Data.Monoid
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Text.Encoding
import           Data.Time
import           GHC.Generics
import           Network.PagerDuty.Internal
import           Network.PagerDuty.IO
import           Network.PagerDuty.Types


type A a = Authenticated a

bASE_PATH :: ByteString
bASE_PATH = "/api/v1/services/"

createService :: CreateService -> PagerDuty (A a) (Either Error ServiceDetails)
createService = fmap (fmap service) . request defaultRequest
    { method = methodPost
    , path   = bASE_PATH
    }
    . CreateServiceRequest

updateService :: ServiceId
              -> UpdateService
              -> PagerDuty (A a) (Either Error ServiceDetails)
updateService (Id sid) = fmap (fmap service) . request defaultRequest
    { method = methodPut
    , path   = bASE_PATH <> encodeUtf8 sid
    }
    . UpdateServiceRequest

listServices :: PagerDuty (A a) (Either Error [ServiceDetails])
listServices = fmap (fmap services) . request defaultRequest
    { method      = methodGet
    , path        = bASE_PATH
    , queryString = "?include[]=escalation_policy&include[]=email_filters"
    } $ T.empty

getService :: ServiceId -> PagerDuty (A a) (Either Error ServiceDetails)
getService (Id sid) = fmap (fmap service) . request defaultRequest
    { method      = methodGet
    , path        = bASE_PATH <> encodeUtf8 sid
    , queryString = "?include[]=escalation_policy&include[]=email_filters"
    } $ T.empty

deleteService :: ServiceId -> PagerDuty (A a) (Either Error Empty)
deleteService (Id sid) = request defaultRequest
    { method = methodDelete
    , path   = bASE_PATH <> encodeUtf8 sid
    } $ T.empty

disableService :: ServiceId
               -> RequesterId
               -> PagerDuty (A Token) (Either Error Empty)
disableService (Id sid) (Id rid) = request defaultRequest
    { method      = methodPut
    , path        = bASE_PATH <> encodeUtf8 sid <> "/disable"
    , queryString = "?requester_id=" <> encodeUtf8 rid
    } $ T.empty

disableService' :: ServiceId -> PagerDuty (A BasicAuth) (Either Error Empty)
disableService' (Id sid) = request defaultRequest
    { method = methodPut
    , path   = bASE_PATH <> encodeUtf8 sid <> "/disable"
    } $ T.empty

enableService :: ServiceId
              -> RequesterId
              -> PagerDuty (A Token) (Either Error Empty)
enableService (Id sid) (Id rid) = request defaultRequest
    { method      = methodPut
    , path        = bASE_PATH <> encodeUtf8 sid <> "/enable"
    , queryString = "?requester_id=" <> encodeUtf8 rid
    } $ T.empty

enableService' :: ServiceId -> PagerDuty (A BasicAuth) (Either Error Empty)
enableService' (Id sid) = request defaultRequest
    { method = methodPut
    , path   = bASE_PATH <> encodeUtf8 sid <> "/enable"
    } $ T.empty

regenerateServiceKey :: ServiceId -> PagerDuty (A a) (Either Error ServiceKey)
regenerateServiceKey (Id sid) = fmap (fmap (sd_service_key . service)) . request defaultRequest
    { method = methodPost
    , path   = bASE_PATH <> encodeUtf8 sid <> "/regenerate_key"
    } $ T.empty


newtype CreateServiceRequest = CreateServiceRequest
    { csr_service :: CreateService
    } deriving (Show, Generic)

instance ToJSON CreateServiceRequest where toJSON = gToJson "csr_"


newtype UpdateServiceRequest = UpdateServiceRequest
    { usr_service :: UpdateService
    } deriving (Show, Generic)

instance ToJSON UpdateServiceRequest where toJSON = gToJson "usr_"


newtype DetailsListResponse = DLR { services :: [ServiceDetails] }
    deriving (Show, Generic)

instance FromJSON DetailsListResponse


newtype DetailsResponse = DR { service :: ServiceDetails }
    deriving (Show, Generic)

instance FromJSON DetailsResponse


data CreateService = CreateService
    { cs_name                    :: !Text
    , cs_escalation_policy_id    :: !Text
    , cs_type                    :: !ServiceType
    , cs_description             :: Maybe Text
    , cs_acknowledgement_timeout :: Maybe Int
    , cs_auto_resolve_timeout    :: Maybe Int
    , cs_severity_filter         :: Maybe Text
    } deriving (Show, Generic)

instance ToJSON CreateService where toJSON = gToJson "cs_"


data UpdateService = UpdateService
    { us_name                    :: Maybe Text
    , us_escalation_policy_id    :: Maybe Text
    , us_type                    :: Maybe ServiceType
    , us_description             :: Maybe Text
    , us_acknowledgement_timeout :: Maybe Int
    , us_auto_resolve_timeout    :: Maybe Int
    , us_severity_filter         :: Maybe Text
    } deriving (Show, Generic)

instance ToJSON UpdateService where toJSON = gToJson "us_"


data ServiceDetails = ServiceDetails
    { sd_id                      :: !ServiceId
    , sd_name                    :: !Text
    , sd_description             :: !Text
    , sd_service_url             :: !Text
    , sd_service_key             :: !ServiceKey
    , sd_auto_resolve_timeout    :: !Int
    , sd_acknowledgement_timeout :: !Int
    , sd_created_at              :: !ZonedTime
    , sd_deleted_at              :: Maybe ZonedTime
    , sd_status                  :: !ServiceStatus
    , sd_last_incident_timestamp :: Maybe ZonedTime
    , sd_email_incident_creation :: Maybe EmailIncidentCreation
    , sd_incident_counts         :: !IncidentCounts
    , sd_email_filter_mode       :: !EmailFilterMode
    , sd_type                    :: !ServiceType
    , sd_escalation_policy       :: Maybe Object -- todo
    , sd_email_filters           :: Maybe Object -- todo
    , sd_severity_filter         :: Maybe Text   -- todo?
    } deriving (Show, Generic)

instance FromJSON ServiceDetails where parseJSON = gFromJson "sd_"

data ServiceStatus
    = Active
    | Warning
    | Critical
    | Maintenance
    | Disabled
    deriving (Eq, Show)

instance FromJSON ServiceStatus where
    parseJSON (String s) = case s of
        "active"      -> pure Active
        "warning"     -> pure Warning
        "critical"    -> pure Critical
        "maintenance" -> pure Maintenance
        "disabled"    -> pure Disabled
        _             -> mzero

    parseJSON _ = mzero


data IncidentCounts = IncidentCounts
    { triggered    :: !Int
    , acknowledged :: !Int
    , resolved     :: !Int
    , total        :: !Int
    } deriving (Show, Generic)

instance FromJSON IncidentCounts


data EmailIncidentCreation
    = OnNewEmail
    | OnNewEmailSubject
    | OnlyIfNoOpenIncidents
    deriving (Eq, Show)

instance FromJSON EmailIncidentCreation where
    parseJSON (String s) = case s of
        "on_new_email"              -> pure OnNewEmail
        "on_new_email_subject"      -> pure OnNewEmailSubject
        "only_if_no_open_incidents" -> pure OnlyIfNoOpenIncidents
        _                           -> mzero

    parseJSON _ = mzero


data EmailFilterMode
    = AllEmail
    | OrRulesEmail
    | AndRulesEmail
    deriving (Eq, Show)

instance FromJSON EmailFilterMode where
    parseJSON (String s) = case s of
        "all-email"       -> pure AllEmail
        "or-rules-email"  -> pure OrRulesEmail
        "and-rules-email" -> pure AndRulesEmail
        _                 -> mzero

    parseJSON _ = mzero


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

instance FromJSON ServiceType where
    parseJSON (String s) = case s of
        "cloudkick"          -> pure CloudKick
        "generic_email"      -> pure GenericEmail
        "generic_events_api" -> pure GenericEventsApi
        "keynote"            -> pure Keynote
        "nagios"             -> pure Nagios
        "pingdom"            -> pure Pingdom
        "server_density"     -> pure ServerDensity
        "sql_monitor"        -> pure SqlMonitor
        _                    -> mzero

    parseJSON _ = mzero

instance ToJSON ServiceType where
    toJSON st = String $ case st of
        CloudKick        -> "cloudkick"
        GenericEmail     -> "generic_email"
        GenericEventsApi -> "generic_events_api"
        Keynote          -> "keynote"
        Nagios           -> "nagios"
        Pingdom          -> "pingdom"
        ServerDensity    -> "server_density"
        SqlMonitor       -> "sql_monitor"
