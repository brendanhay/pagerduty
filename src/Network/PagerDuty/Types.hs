-- Module      : Network.PagerDuty.Types
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.PagerDuty.Types
    (
    -- * Configuration
    -- ** Environment
      Env
    , envDomain
    , envAuth
    , envManager
    , envLogger

    -- ** Logging
    , Logger     (..)
    , debug

    -- * Types
    -- ** Requests
    , Paginate
    , Request

    -- ** Errors
    , Error
    , _Internal
    , _Integration
    , _REST
    -- *** Messages
    , HasMessage (..)
    -- *** Fields
    , HasErrors  (..)
    -- *** Integration
    , IntegrationError
    , status
    -- *** REST
    , RESTError
    , Code
    , code
    , description

    -- ** Authentication
    , Security   (..)
    , Auth       (..)

    -- ** Keys
    , Key
    , mkKey
    -- *** Aliases
    , ServiceKey
    , IncidentKey

    -- ** Identifiers
    , Id
    , mkId
    -- *** Aliases
    , AlertId
    , ContactId
    , EmailFilterId
    , EscalationPolicyId
    , EscalationRuleId
    , IncidentId
    , LogEntryId
    , NoteId
    , NotificationRuleId
    , OverrideId
    , RequesterId
    , ScheduleId
    , ServiceId
    , UserId
    , VendorId
    , WebhookId
    , WindowId

    -- ** SubDomain
    , SubDomain
    , mkSubDomain

    -- ** Address
    , Address
    , mkAddress

    -- ** Miscellaneous
    , Empty      (..)
    ) where

import           Network.PagerDuty.Internal.Types
