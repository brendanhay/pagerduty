{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Module      : Network.PagerDuty.REST.Users.ContactMethods
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Contact methods describe the various channels (phone numbers, SMS numbers
-- and email addresses) used to contact a user when an incident is assigned to
-- the user.
--
-- Access and manipulate the contact methods for a user.
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/users/contact_methods>
module Network.PagerDuty.REST.Users.ContactMethods
    (
    ) where

import Control.Lens
import Data.Monoid
import Data.Text                    (Text)
import Data.Time
import Network.HTTP.Types
import Network.PagerDuty.TH
import Network.PagerDuty.Types

users :: Path
users = "users"

