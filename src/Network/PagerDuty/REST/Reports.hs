{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

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
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/reports>
module Network.PagerDuty.REST.Reports
    (
    -- * Alerts Per Time
      alertsPerTime

    -- * Incidents Per Time
    , incidentsPerTime

    -- * Types
    , Report
    , rSince
    , rUntil
    , rRollup

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
import Data.Time
import Network.PagerDuty.TH
import Network.PagerDuty.Types

default (Builder)

reports :: Path
reports = "reports"

data Rollup
    = Daily
    | Weekly
    | Monthly
      deriving (Eq, Show)

deriveNullary ''Rollup

data Report = Report
    { _rSince'  :: Date
    , _rUntil'  :: Date
    , _rRollup' :: !Rollup
    } deriving (Eq, Show)

queryRequest ''Report

-- | Start of the date range over which you want to search. The time element
-- is optional.
rSince :: Lens' (Request Report s b) UTCTime
rSince = upd.rSince'._D

-- | The end of the date range over which you want to search. This should be
-- in the same format as since.
rUntil :: Lens' (Request Report s b) UTCTime
rUntil = upd.rUntil'._D

-- | Specifies the bucket duration for each summation.
--
-- /Default:/ monthly.
--
-- Example: A time window of two years (based on since and until) with a
-- rollup of monthly will result in 24 sets of data points being returned
-- (one for each month in the span).
rRollup :: Lens' (Request Report s b) Rollup
rRollup = upd.rRollup'

data AlertReport = AlertReport
    { _arStart               :: Date
    , _arEnd                 :: Date
    , _arNumberOfAlerts      :: !Int
    , _arNumberOfPhoneAlerts :: !Int
    , _arNumberOfSmsAlerts   :: !Int
    , _arNumberOfEmailAlerts :: !Int
    } deriving (Eq, Show)

deriveJSON ''AlertReport

arStart :: Lens' AlertReport UTCTime
arStart = lens _arStart (\r x -> r { _arStart = x })  ._D

arEnd :: Lens' AlertReport UTCTime
arEnd = lens _arEnd (\r x -> r { _arEnd = x })  ._D

makeLens "_arNumberOfAlerts"      ''AlertReport
makeLens "_arNumberOfPhoneAlerts" ''AlertReport
makeLens "_arNumberOfSmsAlerts"   ''AlertReport
makeLens "_arNumberOfEmailAlerts" ''AlertReport

data AlertTotals = AlertTotals
    { _atAlerts                      :: [AlertReport]
    , _atTotalNumberOfAlerts         :: !Int
    , _atTotalNumberOfPhoneAlerts    :: !Int
    , _atTotalNumberOfSmsAlerts      :: !Int
    , _atTotalNumberOfEmailAlerts    :: !Int
    , _atTotalNumberOfBillableAlerts :: !Int
    } deriving (Eq, Show)

deriveRecord ''AlertTotals

-- | Get high level statistics about the number of alerts (SMSes, phone calls and
-- emails) sent for the desired time period, summed daily, weekly or monthly.
--
-- @GET \/reports\/alerts_per_time@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/reports/alerts_per_time>
alertsPerTime :: UTCTime -- ^ 'rSince'
              -> UTCTime -- ^ 'rUntil'
              -> Request Report s AlertTotals
alertsPerTime s u =
    mk Report
        { _rSince'  = D s
        , _rUntil'  = D u
        , _rRollup' = Monthly
        } & path .~ reports % "alerts_per_time"

data IncidentReport = IncidentReport
    { _irStart             :: Date
    , _irEnd               :: Date
    , _irNumberOfIncidents :: !Int
    } deriving (Eq, Show)

deriveJSON ''IncidentReport

irStart :: Lens' IncidentReport UTCTime
irStart = lens _irStart (\r x -> r { _irStart = x }) . _D

irEnd :: Lens' IncidentReport UTCTime
irEnd = lens _irEnd (\r x -> r { _irEnd = x }) . _D

makeLens "_irNumberOfIncidents" ''IncidentReport

-- | Get high level statistics about the number of incidents created for the
-- desired time period, summed daily, weekly or monthly.
--
-- @GET \/reports\/incidents_per_time@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/reports/incidents_per_time>
incidentsPerTime :: UTCTime -- ^ 'rSince'
                 -> UTCTime -- ^ 'rUntil'
                 -> Request Report s [IncidentReport]
incidentsPerTime s u =
    mk Report
        { _rSince'  = D s
        , _rUntil'  = D u
        , _rRollup' = Monthly
        } & path   .~ reports % "incidents_per_time"
          & unwrap .~ key "incidents"
