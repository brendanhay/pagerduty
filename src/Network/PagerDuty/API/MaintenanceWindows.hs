{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.PagerDuty.API.MaintenanceWindows
    ( CreateMaintenanceWindow         (..)
    , MaintenanceWindowCounts         (..)
    , MaintenanceWindowCreatorDetails (..)
    , MaintenanceWindowCreatorId
    , MaintenanceWindowDetails        (..)
    , MaintenanceWindowFilter         (..)
    , MaintenanceWindowId
    , ServiceInfo                     (..)
    , UpdateMaintenanceWindow         (..)

    , createMaintenanceWindow
    , createMaintenanceWindow'
    , updateMaintenanceWindow
    , listMaintenanceWindows
    , getMaintenanceWindow
    , deleteMaintenaceWindow
    )
where

import           Control.Applicative
import           Data.Aeson
import           Data.ByteString            (ByteString)
import           Data.Maybe
import           Data.Monoid
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Text.Encoding
import           Data.Time                  (UTCTime)
import           GHC.Generics
import           Network.PagerDuty.Internal
import           Network.PagerDuty.IO
import           Network.PagerDuty.Types


type A a = Authenticated a

bASE_PATH :: ByteString
bASE_PATH = "/api/v1/maintenance_windows/"

createMaintenanceWindow :: CreateMaintenanceWindow
                        -> RequesterId
                        -> PagerDuty (A Token) (Either Error MaintenanceWindowDetails)
createMaintenanceWindow mw rid =
      fmap (fmap dr_maintenance_window)
    . request defaultRequest
          { method = methodPost
          , path   = bASE_PATH
          }
    $ CreateMaintenanceWindowRequest
          { cmwr_maintenance_window = mw
          , cmwr_requester_id       = Just rid
          }


createMaintenanceWindow' :: CreateMaintenanceWindow
                         -> PagerDuty (A BasicAuth) (Either Error MaintenanceWindowDetails)
createMaintenanceWindow' mw =
      fmap (fmap dr_maintenance_window)
    . request defaultRequest
          { method = methodPost
          , path   = bASE_PATH
          }
    $ CreateMaintenanceWindowRequest
          { cmwr_maintenance_window = mw
          , cmwr_requester_id       = Nothing
          }


updateMaintenanceWindow :: MaintenanceWindowId
                        -> UpdateMaintenanceWindow
                        -> PagerDuty (A a) (Either Error MaintenanceWindowDetails)
updateMaintenanceWindow (Id mwid) =
      fmap (fmap dr_maintenance_window)
    . request defaultRequest
          { method = methodPut
          , path   = bASE_PATH <> encodeUtf8 mwid
          }


listMaintenanceWindows :: Maybe Text
                       -> [ServiceId]
                       -> Maybe MaintenanceWindowFilter
                       -> PagerDuty (A a) (Either Error (MaintenanceWindowCounts, [MaintenanceWindowDetails]))
listMaintenanceWindows qry sids filt =
      fmap (fmap (liftA2 (,) lr_counts lr_maintenance_windows))
    . request defaultRequest
          { method      = methodGet
          , path        = bASE_PATH
          , queryString = renderSimpleQuery True mkQuery
          }
    $ T.empty
  where
    mkQuery :: [(ByteString, ByteString)]
    mkQuery = (catMaybes [ fmap ((,) "query"  . encodeUtf8) qry
                         , fmap ((,) "filter" . filterToBS) filt
                         ])
           ++ map (\ (Id sid) -> ("service_ids[]", encodeUtf8 sid)) sids

getMaintenanceWindow :: MaintenanceWindowId
                     -> PagerDuty (A a) (Either Error MaintenanceWindowDetails)
getMaintenanceWindow (Id mwid) =
      fmap (fmap dr_maintenance_window)
    . request defaultRequest
          { method = methodGet
          , path   = bASE_PATH <> encodeUtf8 mwid
          }
    $ T.empty

deleteMaintenaceWindow :: MaintenanceWindowId
                       -> PagerDuty (A a) (Either Error Empty)
deleteMaintenaceWindow (Id mwid) = request defaultRequest
    { method = methodDelete
    , path   = bASE_PATH <> encodeUtf8 mwid
    } $ T.empty

data MaintenanceWindow
data MaintenanceWindowCreator

type MaintenanceWindowId        = Id MaintenanceWindow
type MaintenanceWindowCreatorId = Id MaintenanceWindowCreator


data CreateMaintenanceWindow = CreateMaintenanceWindow
    { cmw_start_time  :: !UTCTime
    , cmw_end_time    :: !UTCTime
    , cmw_description :: Maybe Text
    , cmw_service_ids :: [ServiceId]
    } deriving (Show, Generic)

instance ToJSON CreateMaintenanceWindow where toJSON = gToJson "cmw_"


data UpdateMaintenanceWindow = UpdateMaintenanceWindow
    { umw_start_time  :: Maybe UTCTime
    , umw_end_time    :: Maybe UTCTime
    , umw_description :: Maybe Text
    , umw_service_ids :: [ServiceId]
    } deriving (Show, Generic)

instance ToJSON UpdateMaintenanceWindow where toJSON = gToJson "umw_"


data CreateMaintenanceWindowRequest = CreateMaintenanceWindowRequest
    { cmwr_maintenance_window :: !CreateMaintenanceWindow
    , cmwr_requester_id       :: Maybe RequesterId
    } deriving (Show, Generic)

instance ToJSON CreateMaintenanceWindowRequest where toJSON = gToJson "cmwr_"


data MaintenanceWindowDetails = MaintenanceWindowDetails
    { mwd_id              :: !MaintenanceWindowId
    , mwd_sequence_number :: !Int
    , mwd_start_time      :: !UTCTime
    , mwd_end_time        :: !UTCTime
    , mwd_description     :: Maybe Text
    , mwd_created_by      :: !MaintenanceWindowCreatorDetails
    , mwd_services        :: [ServiceInfo]
    } deriving (Show, Generic)

instance FromJSON MaintenanceWindowDetails where parseJSON = gFromJson "mwd_"


-- possibly used elsewhere (UserDetails or somesuch)
data MaintenanceWindowCreatorDetails = MaintenanceWindowCreatorDetails
    { mwcd_id              :: !MaintenanceWindowCreatorId
    , mwcd_name            :: !Text
    , mwcd_email           :: !Text
    , mwcd_time_zone       :: !Text
    , mwcd_color           :: !Text -- wtf?
    , mwcd_role            :: !Text
    , mwcd_avatar_url      :: !Text
    , mwcd_user_url        :: !Text
    , mwcd_invitation_sent :: !Bool
    } deriving (Show, Generic)

instance FromJSON MaintenanceWindowCreatorDetails where parseJSON = gFromJson "mwcd_"


data ServiceInfo = ServiceInfo
    { si_name :: !Text
    , si_url  :: !Text
    , si_id   :: !ServiceId
    } deriving (Show, Generic)

instance FromJSON ServiceInfo where parseJSON = gFromJson "si_"


data ListResponse = ListResponse
    { lr_maintenance_windows :: [MaintenanceWindowDetails]
    , lr_query               :: Maybe Text
    , lr_counts              :: !MaintenanceWindowCounts
    } deriving (Show, Generic)

instance FromJSON ListResponse where parseJSON = gFromJson "lr_"


data MaintenanceWindowCounts = MaintenanceWindowCounts
    { mwc_ongoing :: !Int
    , mwc_future  :: !Int
    , mwc_past    :: !Int
    , mwc_all     :: !Int
    } deriving (Show, Generic)

instance FromJSON MaintenanceWindowCounts where parseJSON = gFromJson "mwc_"


newtype DetailsResponse = DetailsResponse
    { dr_maintenance_window :: MaintenanceWindowDetails
    } deriving (Show, Generic)

instance FromJSON DetailsResponse where parseJSON = gFromJson "dr_"


data MaintenanceWindowFilter
    = Past
    | Future
    | Ongoing
    deriving Show

filterToBS :: MaintenanceWindowFilter -> ByteString
filterToBS Past    = "past"
filterToBS Future  = "future"
filterToBS Ongoing = "ongoing"
