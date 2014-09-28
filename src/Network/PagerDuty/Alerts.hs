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

import           Control.Applicative
import           Control.Lens            hiding ((.=))
import           Data.Aeson              hiding (Error)
import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8   as BS
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as Map
import           Data.Monoid
import           Data.String
import           Data.Text               (Text)
-- import qualified Network.HTTP.Client   as Client
import           Network.PagerDuty.TH
import           Network.PagerDuty.Types hiding (req)
import qualified Network.PagerDuty.Types as Types
import           Network.HTTP.Types

req :: ToJSON a => StdMethod -> a -> Request a s r
req m = Types.req m (v1 "alerts")

data Filter
    = SMS
    | Email
    | Phone
    | Push
      deriving (Eq, Show)

deriveToJSON ''Filter

data GetAlerts = GetAlerts
    { _since    :: Date -- ^ Fix this relating to time zones etc.
    , _until    :: Date
    , _filter   :: Maybe Filter
    , _timeZone :: Maybe TimeZone
    } deriving (Eq, Show)

deriveToJSON ''GetAlerts

-- | List existing alerts for a given time range, optionally filtered by type
-- (SMS, Email, Phone, or Push).
getAlerts :: Date -- ^ 'since'
          -> Date -- ^ 'until'
          -> Request GetAlerts Token [()]
getAlerts s u = req GET $ GetAlerts
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

-- | Returns only the alerts of the said 'Filter' type.
filter :: Lens' GetAlerts (Maybe Filter)
filter = lens _filter (\s a -> s { _filter = a })

-- | Time zone in which dates in the result will be rendered.
--
-- Defaults to account time zone.
timeZone :: Lens' GetAlerts (Maybe TimeZone)
timeZone = lens _timeZone (\s a -> s { _timeZone = a })

-- class Paginate GetAlerts where
--     next rq f = rq^.rqPager

data Alert = Alert
    { _id :: PKN7NBH,
    , _type :: Email,
    , _started_at :: 2013-03-06T15:28:51-05:00,
    , _user :: User
    , _address :: Address
    }

 -- "user": {
 --    "time_zone": "Eastern Time (US & Canada)",
 --    "color": "dark-slate-grey",
 --    "email": "bart@example.com",
 --    "avatar_url": "https://secure.gravatar.com/avatar/6e1b6fc29a03fc3c13756bd594e314f7.png?d=mm&r=PG",
 --    "user_url": "/users/PIJ90N7",
 --    "invitation_sent": true,
 --    "role": "admin",
 --    "name": "Bart Simpson",
 --    "id": "PIJ90N7"
 --  }
