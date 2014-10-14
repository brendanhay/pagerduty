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

contacts :: UserId -> Path
contacts u = "users" % u % "contact_methods"

data Contact = Contact
    { _cId             :: ContactId
    , _cUserId         :: UserId
    , _cLabel          :: Text
    , _cAddress        :: Address
    , _cType           :: ContactType
    , _cSendShortEmail :: Maybe Bool'
    , _cBlacklisted    :: Maybe Bool'
    } deriving (Eq, Show)

data ContactType
    = SMS
    | Phone
    | Email
      deriving (Eq, Show)

deriveNullary ''ContactType

Name    Type    Required        Description
type    String  Yes     The type of the contact method. One of SMS,  email or phone.
address String  Yes     The id of the contact method. For SMS and  phone it is the number, and for email it is the email address.
country_code    Integer No      The number code for your country. Not used for email. Defaults to 1.
label   String  No      A human friendly label for the contact method. (ie: "Home Phone", "Work Email", etc.) Defaults to the type of the contact method and the address (with country code for phone numbers).
send_short_email        Boolean No      Send an abbreviated email message instead of the standard email output. Useful for email-to-SMS gateways and email based pagers. Only valid for email contact methods. Defaults to false.


-- | List existing contact methods for the specified user.
--
-- @GET \/users\/\:user_id\/contact_methods@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/users/contact_methods/list>
listContacts

-- | Get details for a contact method.
--
-- @GET \/users\/\:user_id\/contact_methods\/\:id@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/users/contact_methods/show>
getContact

-- | Create a new contact method for a given user. A contact method can be one
-- of the following types: SMS, email, and phone. push_notification contact
-- methods cannot be created using this API, they are added automatically during
-- device registration.
--
-- @POST \/users\/\:user_id\/contact_methods@
--
-- /Note:/ The contact info must be unique.
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/users/contact_methods/create>
createContact

-- | Update an existing contact method of a given user. Note that you cannot change the type of an existing method.
--
-- @PUT \/users\/\:user_id\/contact_methods\/\:id@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/users/contact_methods/update>
updateContact

-- | Remove a contact method and any corresponding notification rules.
--
-- @DELETE \/users\/\:user_id\/contact_methods\/\:id@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/users/contact_methods/delete>
deleteContact
