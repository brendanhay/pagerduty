{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- Module      : Network.PagerDuty.REST.Schedules
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Schedule overrides are custom, non-recurring exceptions to your regular
-- on-call schedules. Use them when your team members go on vacation, swap shifts,
-- or when you simply cannot achieve your normal scheduling with recurring layers.
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/schedules>
module Network.PagerDuty.REST.Schedules.Overrides
    (
    -- * List Overrides
      ListOverrides
    , listOverrides
    , lsSince
    , lsUntil
    , lsEditable
    , lsOverflow

    -- * Create Override
    , CreateOverride
    , createOverride
    , coUserId
    , coStart
    , coEnd

    -- * Delete Override
    , deleteOverride

    -- * Types
    , HasUserInfo (..)
    , Override
    , oId
    , oStart
    , oEnd
    , oUser
    ) where

import           Control.Lens
import           Data.Time
import           Network.HTTP.Types
import           Network.PagerDuty.Internal.TH
import           Network.PagerDuty.Internal.Types
import           Network.PagerDuty.REST.Users

overrides :: ScheduleId -> Path
overrides i = "schedules" % i % "overrides"

data Override = Override
    { _oId    :: Maybe OverrideId
    , _oStart :: Date
    , _oEnd   :: Date
    , _oUser  :: UserInfo
    } deriving (Eq, Show)

deriveRecord ''Override

instance HasUserInfo Override where
    userInfo = oUser

data ListOverrides = ListOverrides
    { _lsSince'    :: Date
    , _lsUntil'    :: Date
    , _lsEditable' :: Bool'
    , _lsOverflow' :: Bool'
    } deriving (Eq, Show)

queryRequest ''ListOverrides

-- | The start time of the date range you want to retrieve override for.
--
-- The maximum date range queryable is 3 months.
lsSince :: Lens' (Request ListOverrides s b) UTCTime
lsSince = upd.lsSince'._D

-- | The end time of the date range you want to retrieve override for.
lsUntil :: Lens' (Request ListOverrides s b) UTCTime
lsUntil = upd.lsUntil'._D

-- | When 'True', only editable overrides will be returned.
--
-- The result will onlyinclude the id the override if this parameter is present.
-- Only future overrides are editable.
--
-- /Default:/ 'False.'
lsEditable :: Lens' (Request ListOverrides s b) Bool
lsEditable = upd.lsEditable'._B

-- | Any on-call schedule entries that pass the date range bounds will be
-- truncated at the bounds, unless @lsEditable .~ True@ is specified.
--
-- /Default:/ 'False.'
lsOverflow :: Lens' (Request ListOverrides s b) Bool
lsOverflow = upd.lsOverflow'._B

-- | List overrides for a given time range.
--
-- @GET \/schedules\/\:schedule_id\/overrides@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/schedules/overrides/list>
listOverrides :: ScheduleId
              -> Date -- ^ 'lsSince'
              -> Date -- ^ 'lsUntil'
              -> Request ListOverrides s [Override]
listOverrides i s u =
    mk ListOverrides
        { _lsSince'    = s
        , _lsUntil'    = u
        , _lsEditable' = F
        , _lsOverflow' = F
        } & path .~ overrides i

data CreateOverride = CreateOverride
    { _coStart'  :: Date
    , _coEnd'    :: Date
    , _coUserId' :: UserId
    } deriving (Eq, Show)

jsonRequest ''CreateOverride

-- | The start date and time for the override.
coStart :: Lens' (Request CreateOverride s b) UTCTime
coStart = upd.coStart'._D

-- | The end date and time for the override.
coEnd :: Lens' (Request CreateOverride s b) UTCTime
coEnd = upd.coEnd'._D

-- | The ID of the user who will be on call for the duration of the override.
coUserId :: Lens' (Request CreateOverride s b) UserId
coUserId = upd.coUserId'

-- | Create an override for a specific user covering the specified time range. If
-- you create an override on top of an existing one, the last created override
-- will have priority.
--
-- If no time zone information is present in the since and until parameters, the
-- schedule's time zone will be used.
--
-- /Note:/ You cannot create overrides in the past.
--
-- @POST \/schedules\/\:schedule_id\/overrides@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/schedules/overrides/create>
createOverride :: ScheduleId
               -> UserId -- ^ 'coUserId'
               -> Date   -- ^ 'coStart'
               -> Date   -- ^ 'coEnd'
               -> Request CreateOverride s Override
createOverride i u s e =
    mk CreateOverride
        { _coUserId' = u
        , _coStart'  = s
        , _coEnd'    = e
        } & meth .~ POST
          & path .~ overrides i

-- | Remove an override.
--
-- You cannot remove a past override. If the override start
-- time is before the current time, but the end time is after the current time,
-- the override will be truncated to the current time. If the override is
-- truncated, the status code will be 200 OK, as opposed to a 204 No Content for a
-- successful delete.
--
-- @DELETE \/schedules\/\:schedule_id\/overrides\/\:id@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/schedules/overrides/delete>

-- FIXME: How to parse whether truncated or deleted?
deleteOverride :: ScheduleId -> OverrideId -> Request Empty s Empty
deleteOverride i o = empty
    & meth .~ DELETE
    & path .~ overrides i % o
