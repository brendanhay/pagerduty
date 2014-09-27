{-# LANGUAGE DeriveGeneric #-}

-- Module      : Network.PagerDuty.API.LogEntries
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.PagerDuty.API.LogEntries
where

import Data.Aeson
import GHC.Generics
import Network.PagerDuty.Types


data LogEntries = LogEntries
    { lesTotal      :: !Int
    , leslogEntries :: [LogEntry]
    } deriving (Eq, Show, Generic)

instance FromJSON LogEntries
instance ToJSON   LogEntries

logEntries :: PagerDuty Authenticated (Either Error LogEntries)
logEntries = undefined
