{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Module      : Network.PagerDuty.Alerts.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.PagerDuty.Alerts.Types where

import Control.Lens
import Network.PagerDuty.TH
import Network.PagerDuty.Types

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
