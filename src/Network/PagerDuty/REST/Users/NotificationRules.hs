{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- Module      : Network.PagerDuty.REST.Users.NotificationRules
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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
    -- * List Rules
      listRules

    -- * Create Rule
    , CreateRule
    , createRule
    , crStartDelayInMinutes
    , crContactMethodId

    -- * Get Rule
    , getRule

    -- * Update Rule
    , UpdateRule
    , updateRule
    , urStartDelayInMinutes
    , urContactMethodId

    -- * Delete Rule
    , deleteRule

    -- * Types
    , Rule
    , rId
    , rStartDelayInMinutes
    , rContactMethod
    ) where

import Control.Lens
import Data.Aeson.Lens
import Network.HTTP.Types
import Network.PagerDuty.REST.Users.ContactMethods
import Network.PagerDuty.Internal.TH
import Network.PagerDuty.Internal.Types

default (Path)

rules :: UserId -> Path
rules u = "users" % u % "notification_rules"

data Rule = Rule
    { _rId                  :: NotificationRuleId
    , _rStartDelayInMinutes :: !Int
    , _rContactMethod       :: Contact
    } deriving (Eq, Show)

deriveJSON ''Rule
makeLenses ''Rule

instance HasContact Rule where
    contact = rContactMethod

-- | List existing notification rules for the specified user.
--
-- @GET \/users\/\:user_id\/notification_rules@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/users/notification_rules/list>
listRules :: UserId -> Request Empty s Rule
listRules u = empty & path .~ rules u

-- | Get details for a notification rule.
--
-- @GET \/users\/\:user_id\/notification_rules\/\:id@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/users/notification_rules/show>
getRule :: UserId -> NotificationRuleId -> Request Empty s Rule
getRule u n = empty & path .~ rules u % n

data CreateRule = CreateRule
    { _crStartDelayInMinutes' :: !Int
    , _crContactMethodId'     :: ContactId
    } deriving (Eq, Show)

jsonRequest ''CreateRule

-- | Number of minutes it will take for the notification rule to be activated
-- (from the time the incident is assigned to the owning user) and an alert be
-- fired.
crStartDelayInMinutes :: Lens' (Request CreateRule s b) Int
crStartDelayInMinutes = upd.crStartDelayInMinutes'

-- | The id of the contact method.
crContactMethodId :: Lens' (Request CreateRule s b) ContactId
crContactMethodId = upd.crContactMethodId'

-- | Create a new notification rule for the specified user.
--
-- @POST \/users\/\:user_id\/notification_rules@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/users/notification_rules/create>
createRule :: UserId -> ContactId -> Int -> Request CreateRule s Rule
createRule u c n =
    mk CreateRule
        { _crStartDelayInMinutes' = n
        , _crContactMethodId'     = c
        } & meth   .~ POST
          & path   .~ rules u
          & unwrap .~ key "notification_rule"

data UpdateRule = UpdateRule
    { _urStartDelayInMinutes' :: Maybe Int
    , _urContactMethodId'     :: Maybe ContactId
    } deriving (Eq, Show)

jsonRequest ''UpdateRule

-- | Number of minutes it will take for the notification rule to be activated
-- (from the time the incident is assigned to the owning user) and an alert be
-- fired.
urStartDelayInMinutes :: Lens' (Request UpdateRule s b) (Maybe Int)
urStartDelayInMinutes = upd.urStartDelayInMinutes'

-- | The id of the contact method.
urContactMethodId :: Lens' (Request UpdateRule s b) (Maybe ContactId)
urContactMethodId = upd.urContactMethodId'

-- | Update an existing notification rule.
--
-- @PUT \/users\/\:user_id\/notification_rules\/\:id@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/users/notification_rules/update>
updateRule :: UserId -> NotificationRuleId -> Request UpdateRule s Rule
updateRule u r =
    mk UpdateRule
        { _urStartDelayInMinutes' = Nothing
        , _urContactMethodId'     = Nothing
        } & meth   .~ PUT
          & path   .~ rules u % r
          & unwrap .~ key "notification_rule"

-- | Remove a notification rule.
--
-- @DELETE \/users\/\:user_id\/notification_rules\/\:id@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/users/notification_rules/delete>
deleteRule :: UserId
                       -> NotificationRuleId
                       -> Request Empty s Empty
deleteRule u n = empty & meth .~ DELETE & path .~ rules u % n
