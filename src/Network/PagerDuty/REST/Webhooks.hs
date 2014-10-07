{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Module      : Network.PagerDuty.REST.Webhooks
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Webhooks let you recieve HTTP callbacks when interesting events happen within
-- your PagerDuty account. Details surrounding the interesting event will be sent
-- via HTTP to a URL that you specify.
--
-- PagerDuty currently supports incident-based webhooks. After adding a webhook
-- URL to a PagerDuty service, the triggering of new incidents on that service
-- will cause outgoing webhook messages to be sent to that URL. In addition,
-- certain interesting changes to an incident's state will cause other types of
-- incident webhook messages to be sent: Generally, any change to the @status@ or
-- @assigned_to_user@ of an incident will cause an outgoing message to be sent.
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/webhooks>
module Network.PagerDuty.REST.Webhooks
    (

    ) where

-- Just create the types + fromJSON instances
decodeWebhook = undefined
