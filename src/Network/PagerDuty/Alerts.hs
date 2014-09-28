{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Module      : Network.PagerDuty.Alerts
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Alerts API
--
-- When an incident is triggered or when it is escalated it creates an alert
-- (also known as a notification). Alerts are messages containing the details
-- of the incident, and can be sent through SMS, email, phone calls,
-- and push notifications.
--
-- This API allows you to access read-only data regarding what alerts have been
-- sent to your users.
module Network.PagerDuty.Alerts where

import Control.Lens            hiding ((.=))
import Data.Aeson              (ToJSON)
import Data.Aeson.Lens
import Network.HTTP.Types
import Network.PagerDuty.TH
import Network.PagerDuty.Types

req :: ToJSON a => StdMethod -> Unwrap -> a -> Request a s r
req m u = req' m (v1 "alerts") u

data AlertType
    = SMS
    | Email
    | Phone
    | Push
      deriving (Eq, Show)

deriveJSON ''AlertType

data GetAlerts = GetAlerts
    { _since    :: Date -- ^ Fix this relating to time zones etc.
    , _until    :: Date
    , _filter   :: Maybe AlertType
    , _timeZone :: Maybe TimeZone
    } deriving (Eq, Show)

deriveJSON ''GetAlerts

-- | List existing alerts for a given time range, optionally filtered by type
-- (SMS, Email, Phone, or Push).
getAlerts :: Date -- ^ 'since'
          -> Date -- ^ 'until'
          -> Request GetAlerts Token [Alert]
getAlerts s u = req GET (key "alerts") $
    GetAlerts
        { _since    = s
        , _until    = u
        , _filter   = Nothing
        , _timeZone = Nothing
        }

-- | The start of the date range over which you want to search.
since :: Lens' GetAlerts Date
since = lens _since (\s a -> s { _since = a })

-- | The end of the date range over which you want to search.
-- This should be in the same format as 'since'.
--
-- The size of the date range must be less than 3 months.
until :: Lens' GetAlerts Date
until = lens _until (\s a -> s { _until = a })

-- | Returns only the alerts of the said 'AlertType' type.
filter :: Lens' GetAlerts (Maybe AlertType)
filter = lens _filter (\s a -> s { _filter = a })

-- | Time zone in which dates in the result will be rendered.
--
-- Defaults to account time zone.
timeZone :: Lens' GetAlerts (Maybe TimeZone)
timeZone = lens _timeZone (\s a -> s { _timeZone = a })

instance Paginate GetAlerts

data Alert = Alert
    { _alertId        :: AlertId
    , _alertType      :: AlertType
    , _alertStartedAt :: Date
    , _alertUser      :: User
    , _alertAddress   :: Address
    } deriving (Eq, Show)

deriveJSON ''Alert
