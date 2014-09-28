{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Module      : Network.PagerDuty.EscalationPolicies.List
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | List all the existing escalation policies.
--
-- @GET \/escalation_policies@
--
-- See: <http://developer.pagerduty.com/documentation/rest/escalation_policies/list>
module Network.PagerDuty.EscalationPolicies.List
    ( listPolicies
    , lpQuery
    ) where

import           Data.Aeson.Lens
import qualified Data.ByteString.Char8                      as BS
import           Data.Text                                  (Text)
import           Network.HTTP.Types
import           Network.PagerDuty.EscalationPolicies.Types
import           Network.PagerDuty.TH
import           Network.PagerDuty.Types

newtype ListPolicies = ListPolicies
    { _lpQuery :: Maybe Text
    } deriving (Eq, Show)

listPolicies :: Request ListPolicies Token [Policy]
listPolicies = req GET BS.empty (key "escalation_policies") $
    ListPolicies
        { _lpQuery = Nothing
        }

-- | Filters the result, showing only the escalation policies
-- whose names match the query.
makeLens "_lpQuery" ''ListPolicies

deriveJSON ''ListPolicies

instance Paginate ListPolicies

