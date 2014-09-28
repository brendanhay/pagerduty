{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Module      : Network.PagerDuty.Services.List
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | List existing services.
--
-- @GET \/services@
--
-- See: <http://developer.pagerduty.com/documentation/rest/services/list>
module Network.PagerDuty.Services.List
    ( listServices
    , lsTimeZone
    ) where

import           Control.Lens
import           Data.Aeson.Lens
import qualified Data.ByteString.Char8            as BS
import           Data.Text                        (Text)
import           Network.HTTP.Types
import           Network.PagerDuty.Services.Types
import           Network.PagerDuty.TH
import           Network.PagerDuty.Types

newtype ListServices = ListServices
    { _lsTimeZone :: Maybe Text
    } deriving (Eq, Show)

listServices :: Request ListServices Token [Service]
listServices = req GET BS.empty (key "escalation_policies")
    (ListServices
        { _lsTimeZone = Nothing
        }) & rqQuery <>~ includes

-- | Time zone in which dates in the result will be rendered.
-- Defaults to account default time zone.
makeLens "_lsTimeZone" ''ListServices

deriveJSON ''ListServices

instance Paginate ListServices
