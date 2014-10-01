{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Module      : Network.PagerDuty.REST.Schedules
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | On call schedules determine the time periods that users are on-call. Only when
-- a user is on-call he is eligible to receive alerts from incidents.
--
-- This API allows users to manipulate on-call schedules.
module Network.PagerDuty.REST.Schedules
    (

    ) where

schedules :: Path
schedules = "schedules"
