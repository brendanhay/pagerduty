{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- Module      : Network.PagerDuty.REST.Reports
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Access high level reports about alerts and incidents.
-- Useful for creating graphs.
module Network.PagerDuty.REST.Reports
    (
    -- * Alerts Per Time
      alertsPerTime

    -- * Incidents Per Time
    , incidentsPerTime

    -- * Types
    , Rollup (..)

    , AlertReport
    , arStart
    , arEnd
    , arNumberOfAlerts
    , arNumberOfPhoneAlerts
    , arNumberOfSmsAlerts
    , arNumberOfEmailAlerts

    , AlertTotals
    , atAlerts
    , atTotalNumberOfAlerts
    , atTotalNumberOfPhoneAlerts
    , atTotalNumberOfSmsAlerts
    , atTotalNumberOfEmailAlerts
    , atTotalNumberOfBillableAlerts

    , IncidentReport
    , irStart
    , irEnd
    , irNumberOfIncidents
    ) where

import Control.Lens
import Data.Aeson.Lens
import Data.ByteString.Builder (Builder)
import Data.Text               (Text)
import Network.HTTP.Types
import Network.PagerDuty.TH
import Network.PagerDuty.Types

default (Builder)

reports :: Path
reports = "reports"

data Rollup
    = Daily
    | Weekly
    | Monthly
      deriving (Eq Show)

deriveJSON ''Rollup

data Report = Report
    { _rSince'  :: Date
    , _rUntil'  :: Date
    , _rRollup' :: !Rollup
    } deriving (Eq, Show)

deriveJSON ''Report
makeLenses ''Report

data AlertReport
    { _arStart               :: Date
    , _arEnd                 :: Date
    , _arNumberOfAlerts      :: !Int
    , _arNumberOfPhoneAlerts :: !Int
    , _arNumberOfSmsAlerts   :: !Int
    , _arNumberOfEmailAlerts :: !Int
    } deriving (Eq, Show)

deriveJSON ''AlertReport
makeLenses ''AlertReport

data AlertTotals = AlertTotals
    { _atAlerts                      :: [AlertReport]
    , _atTotalNumberOfAlerts         :: !Int
    , _atTotalNumberOfPhoneAlerts    :: !Int
    , _atTotalNumberOfSmsAlerts      :: !Int
    , _atTotalNumberOfEmailAlerts    :: !Int
    , _atTotalNumberOfBillableAlerts :: !Int
    } deriving (Eq, Show)

deriveJSON ''AlertTotals
makeLenses ''AlertTotals

data IncidentReport = IncidentReport
    { _irStart             :: Date
    , _irEnd               :: Date
    , _irNumberOfIncidents :: !Int
    } deriving (Eq, Show)

deriveJSON ''IncidentReport
makeLenses ''IncidentReport

-- | Start of the date range over which you want to search. The time element
-- is optional.
rSince :: Lens' (Request Report s r) Date
rSince = upd.rSince'

-- | The end of the date range over which you want to search. This should be
-- in the same format as since.
rUntil :: Lens' (Request Report s r) Date
rUntil = upd.rUntil'

-- | Specifies the bucket duration for each summation.
--
-- Defaults to monthly.
--
-- Example: A time window of two years (based on since and until) with a
-- rollup of monthly will result in 24 sets of data points being returned
-- (one for each month in the span).
rSince :: Lens' (Request Report s r) Rollup
rSince = upd.rSince'

-- | Get high level statistics about the number of alerts (SMSes, phone calls and
-- emails) sent for the desired time period, summed daily, weekly or monthly.
--
-- @GET \/reports\/alerts_per_time@
--
-- See: <http://developer.pagerduty.com/documentation/rest/reports/alerts_per_time>
alertsPerTime :: Date -- ^ 'rSince'
              -> Date -- ^ 'rUntil'
              -> Request Report s AlertTotals
alertsPerTime s u =
    mk Report
        { _rSince  = s
        , _rUntil  = u
        , _rRollup = Monthly
        } & path .~ reports % "alerts_per_time"

-- | Get high level statistics about the number of incidents created for the
-- desired time period, summed daily, weekly or monthly.
--
-- @GET \/reports\/incidents_per_time@
--
-- See: <http://developer.pagerduty.com/documentation/rest/reports/incidents_per_time>
incidentsPerTime :: Date -- ^ 'rSince'
                 -> Date -- ^ 'rUntil'
                 -> Request Report s [IncidentReport]
incidentsPerTime s u =
    mk Report
        { _rSince  = s
        , _rUntil  = u
        , _rRollup = Monthly
        } & path .~ reports % "incidents_per_time"
