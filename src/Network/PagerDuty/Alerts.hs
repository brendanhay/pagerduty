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

import           Control.Lens            hiding ((.=))
import           Data.Aeson              (ToJSON)
import           Data.Aeson.Lens
import qualified Data.ByteString         as BS
import           Network.HTTP.Types
import           Network.PagerDuty.TH
import           Network.PagerDuty.Types

req :: ToJSON a => StdMethod -> Unwrap -> a -> Request a s r
req m u = req' m ("alerts", BS.empty) u

data AlertType
    = SMS
    | Email
    | Phone
    | Push
      deriving (Eq, Show)

deriveJSON ''AlertType

data Alert = Alert
    { _alertId        :: AlertId
    , _alertType      :: AlertType
    , _alertStartedAt :: Date
    , _alertUser      :: User
    , _alertAddress   :: Address
    } deriving (Eq, Show)

deriveJSON ''Alert
makeLenses ''Alert

data ListAlerts = ListAlerts
    { _lstSince    :: Date -- ^ Fix this relating to time zones etc.
    , _lstUntil    :: Date
    , _lstFilter   :: Maybe AlertType
    , _lstTimeZone :: Maybe TimeZone
    } deriving (Eq, Show)

-- | List existing alerts for a given time range, optionally filtered by type
-- (SMS, Email, Phone, or Push).
listAlerts :: Date -- ^ 'since'
           -> Date -- ^ 'until'
           -> Request ListAlerts Token [Alert]
listAlerts s u = req GET (key "alerts") $
    ListAlerts
        { _lstSince    = s
        , _lstUntil    = u
        , _lstFilter   = Nothing
        , _lstTimeZone = Nothing
        }

-- | The start of the date range over which you want to search.
makeLens "_lstSince" ''Alert

-- | The end of the date range over which you want to search.
-- This should be in the same format as 'since'.
--
-- The size of the date range must be less than 3 months.
makeLens "_lstUntil" ''Alert

-- | Returns only the alerts of the said 'AlertType' type.
makeLens "_lstFilter" ''Alert

-- | Time zone in which dates in the result will be rendered.
--
-- Defaults to account time zone.
makeLens "_lstTimeZone" ''Alert

deriveJSON ''ListAlerts

instance Paginate ListAlerts
