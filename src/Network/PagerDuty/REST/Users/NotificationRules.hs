{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Module      : Network.PagerDuty.REST.Users.NotificationRules
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | When a user is assigned an incident, the user's notification rules are
-- used to determine which contact method will be used to alert the user and how
-- long after the incident to do so.
--
-- Access and manipulate the notification rules for a user.
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/users/notification_rules>
module Network.PagerDuty.REST.Users.NotificationRules
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

-- | List existing notification rules for the specified user.
--
-- @GET \/users\/\:user_id\/notification_rules@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/users/notification_rules/list>
listNotificationRules

-- | Get details for a notification rule.
--
-- @GET \/users\/\:user_id\/notification_rules\/\:id@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/users/notification_rules/show>
getNotificationRule

-- | Create a new notification rule for the specified user.
--
-- @POST \/users\/\:user_id\/notification_rules@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/users/notification_rules/create>
createNotificationRule

-- | Update an existing notification rule.
--
-- @PUT \/users\/\:user_id\/notification_rules\/\:id@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/users/notification_rules/update>
updateNotificationRule

-- | Remove a notification rule.
--
-- @DELETE \/users\/\:user_id\/notification_rules\/\:id@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/users/notification_rules/delete>
deleteNotificationRule
