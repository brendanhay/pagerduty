{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Module      : Network.PagerDuty.REST.EscalationPolicies.OnCall
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | List all the existing escalation policies with currently on-call users.
--
-- If the start and end of an on-call object are null, then the user is always
-- on-call for an escalation policy level.
module Network.PagerDuty.REST.EscalationPolicies.OnCall
    (
    ) where

import Control.Lens
import Data.Monoid
import Data.Text                    (Text)
import Data.Time
import Network.HTTP.Types
import Network.PagerDuty.TH
import Network.PagerDuty.Types
