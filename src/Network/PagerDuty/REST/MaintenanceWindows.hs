{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.PagerDuty.REST.MaintenanceWindows
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Maintenance windows allow you to schedule service maintenance periods,
-- during which no incidents will be created.
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/maintenance_windows>
module Network.PagerDuty.REST.MaintenanceWindows
    (
    -- * List Windows
      ListWindows
    , listWindows
    , lwQuery
    , lwServiceIds
    , lwFilter

    , ListWindowsResponse
    , lwMaintenanceWindows
    , lwCounts

    -- * Create Window
    , CreateWindow
    , createWindow
    , createWindowBasic
    , cwStartTime
    , cwEndTime
    , cwDescription
    , cwServiceIds

    -- * Get Window
    , getWindow

    -- * Update Window
    , UpdateWindow
    , updateWindow
    , uwStartTime
    , uwEndTime
    , uwDescription
    , uwServiceIds

    -- * Delete Window
    , deleteWindow

    -- * Types
    , ServiceInfo
    , siId
    , siName
    , siUrl

    , Counts
    , cOngoing
    , cFuture
    , cPast
    , cAll

    , MaintenanceWindow
    , mwSequenceNumber
    , mwStartTime
    , mwEndTime
    , mwDescription
    , mwCreatedBy
    , mwServices
    ) where

import           Control.Lens                     hiding (Empty)
import           Data.Monoid
import           Data.Text                        (Text)
import           Data.Time
import           Network.HTTP.Types
import           Network.PagerDuty.Internal.TH
import           Network.PagerDuty.Internal.Types
import           Network.PagerDuty.REST.Users     (User)

windows :: Path
windows = "maintenance_windows"

data ServiceInfo = ServiceInfo
    { _siId   :: ServiceId
    , _siName :: Text
    , _siUrl  :: Text
    } deriving (Eq, Show)

deriveRecord ''ServiceInfo

data MaintenanceWindow = MaintenanceWindow
    { _mwSequenceNumber :: !Int
    , _mwStartTime      :: Date
    , _mwEndTime        :: Date
    , _mwDescription    :: Maybe Text
    , _mwCreatedBy      :: User
    , _mwServices       :: [ServiceInfo]
    } deriving (Eq, Show)

deriveRecord ''MaintenanceWindow

data ListWindows = ListWindows
    { _lwQuery'      :: Maybe Text
    , _lwServiceIds' :: List ServiceId
    , _lwFilter'     :: Maybe Text
    }

queryRequest ''ListWindows

-- | Filters the results, showing only the maintenance windows whose
-- descriptions contain the query.
lwQuery :: Lens' (Request ListWindows s b) (Maybe Text)
lwQuery = upd.lwQuery'

-- | An list of service IDs, specifying services whose maintenance
-- windows shall be returned.
lwServiceIds :: Lens' (Request ListWindows s b) [ServiceId]
lwServiceIds = upd.lwServiceIds'._L

-- | Only return maintenance windows that are of this type. Possible values
-- are past, future, ongoing. If this parameter is omitted, all maintenance
-- windows will be returned.
lwFilter :: Lens' (Request ListWindows s b) (Maybe Text)
lwFilter = upd.lwFilter'

data Counts = Counts
    { _cOngoing :: !Int
    , _cFuture  :: !Int
    , _cPast    :: !Int
    , _cAll     :: !Int
    } deriving (Eq, Show)

deriveRecord ''Counts

data ListWindowsResponse = ListWindowsResponse
    { _lwMaintenanceWindows :: [MaintenanceWindow]
    , _lwCounts             :: Counts
    } deriving (Eq, Show)

deriveRecord ''ListWindowsResponse

-- | List existing maintenance windows, optionally filtered by service,
-- or whether they are from the past, present or future.
--
-- @GET \/maintenance_windows@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/maintenance_windows/list>
listWindows :: Request ListWindows s ListWindowsResponse
listWindows =
    mk ListWindows
        { _lwQuery'      = Nothing
        , _lwServiceIds' = mempty
        , _lwFilter'     = Nothing
        } & path .~ windows

data CreateWindow' = CreateWindow'
    { _cwStartTime'   :: Date
    , _cwEndTime'     :: Date
    , _cwDescription' :: Maybe Text
    , _cwServiceIds'  :: [ServiceId] -- ^ List1
    } deriving (Eq, Show)

deriveRecord ''CreateWindow'

data CreateWindow = CreateWindow
    { _cwRequesterId'       :: Maybe RequesterId
    , _cwMaintenanceWindow' :: CreateWindow'
    } deriving (Eq, Show)

jsonRequest ''CreateWindow

-- | This maintenance window's start time. This is when the services will stop
-- creating incidents. If this date is in the past, it will be updated to be the
-- current time.
cwStartTime :: Lens' (Request CreateWindow s b) UTCTime
cwStartTime = upd.cwMaintenanceWindow'.cwStartTime'._D

-- | This maintenance window's end time. This is when the services will start
-- creating incidents again. This date must be in the future and after the
-- start_time.
cwEndTime :: Lens' (Request CreateWindow s b) UTCTime
cwEndTime = upd.cwMaintenanceWindow'.cwEndTime'._D

-- | A description for this maintenance window.
cwDescription :: Lens' (Request CreateWindow s b) (Maybe Text)
cwDescription = upd.cwMaintenanceWindow'.cwDescription'

-- | The ids of the services that are affected by this maintenance window.
cwServiceIds :: Lens' (Request CreateWindow s b) [ServiceId]
cwServiceIds = upd.cwMaintenanceWindow'.cwServiceIds'

-- | Create a new maintenance window for the specified services. No new
-- incidents will be created for a service that is currently in maintenance.
--
-- @POST \/maintenance_windows@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/maintenance_windows/create>
createWindow :: RequesterId
             -> UTCTime     -- ^ 'cwStartTime'
             -> UTCTime     -- ^ 'cwEndTime'
             -> [ServiceId] -- ^ 'cwServiceIds'
             -> Request CreateWindow s MaintenanceWindow
createWindow r s e xs =
    auth (createWindowBasic s e xs) & upd.cwRequesterId' ?~ r

-- | A version of 'createWindow' which uses HTTP Basic authentication and
-- doesn't require a 'RequesterId'.
createWindowBasic :: UTCTime     -- ^ 'cwStartTime'
                  -> UTCTime     -- ^ 'cwEndTime'
                  -> [ServiceId] -- ^ 'cwServiceIds'
                  -> Request CreateWindow 'Basic MaintenanceWindow
createWindowBasic s e xs =
    mk CreateWindow
        { _cwRequesterId'       = Nothing
        , _cwMaintenanceWindow' = CreateWindow'
            { _cwStartTime'   = D s
            , _cwEndTime'     = D e
            , _cwDescription' = Nothing
            , _cwServiceIds'  = xs
            }
        } & meth .~ POST
          & path .~ windows

-- | Get details about an existing maintenance window.
--
-- @GET \/maintenance_windows\/\:id@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/maintenance_windows/show>
getWindow :: WindowId -> Request Empty s MaintenanceWindow
getWindow w = empty & path .~ windows % w

data UpdateWindow = UpdateWindow
    { _uwStartTime'   :: Maybe Date
    , _uwEndTime'     :: Maybe Date
    , _uwDescription' :: Maybe Text
    , _uwServiceIds'  :: Maybe [ServiceId]
    } deriving (Eq, Show)

jsonRequest ''UpdateWindow

-- | The maintenance window's start time. Can only be updated on future
-- maintenance windows. If the start_time is set to a date in the past, it will be
-- updated to the current date.
uwStartTime :: Lens' (Request UpdateWindow s b) (Maybe Date)
uwStartTime = upd.uwStartTime'

-- | The maintenance window's end time. Can only be updated on ongoing and
-- future maintenance windows, and cannot be set to a value before start_time.
uwEndTime :: Lens' (Request UpdateWindow s b) (Maybe Date)
uwEndTime = upd.uwEndTime'

-- | Description for this maintenance window. Can only be updated on ongoing
-- and future maintenance windows.
uwDescription :: Lens' (Request UpdateWindow s b) (Maybe Text)
uwDescription = upd.uwDescription'

-- | Services that are affected by this maintenance window. Can only be updated
-- on future maintenance windows.
uwServiceIds :: Lens' (Request UpdateWindow s b) (Maybe [ServiceId])
uwServiceIds = upd.uwServiceIds'

-- | Update an existing maintenance window.
--
-- @PUT \/maintenance_windows\/\:id@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/maintenance_windows/update>
updateWindow :: WindowId -> Request UpdateWindow s MaintenanceWindow
updateWindow w =
    mk UpdateWindow
         { _uwStartTime'   = Nothing
         , _uwEndTime'     = Nothing
         , _uwDescription' = Nothing
         , _uwServiceIds'  = Nothing
         } & meth .~ PUT
           & path .~ windows % w

-- | Cancel or delete an existing maintenance window.
--
-- @DELETE \/maintenance_windows\/\:id@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/maintenance_windows/delete>
deleteWindow :: WindowId -> Request Empty s Empty
deleteWindow w = empty & meth .~ DELETE & path .~ windows % w
