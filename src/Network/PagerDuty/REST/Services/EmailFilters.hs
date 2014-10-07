{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Module      : Network.PagerDuty.REST.Services.EmailFilters
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Email Filters are a set of rules that are applied to triggering email's body,
-- subject and from address. It only applies to generic_email kind of
-- Services. The way multiple filters are combined depends on the
-- email_filter_mode attribute of the service.
module Network.PagerDuty.REST.Services.EmailFilters
    (
    ) where

import Control.Lens
import Data.Monoid
import Data.Text                    (Text)
import Data.Time
import Network.HTTP.Types
import Network.PagerDuty.TH
import Network.PagerDuty.Types

