{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- Module      : Network.PagerDuty.REST.Users.ContactMethods
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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
    -- * List Contacts
      listContacts

    -- * Create Contact
    , CreateContact
    , createContact
    , ccType
    , ccAddress
    , ccCountryCode
    , ccLabel
    , ccSendShortEmail

    -- * Get Contact
    , getContact

    -- * Update Contact
    , UpdateContact
    , updateContact
    , ucAddress
    , ucCountryCode
    , ucLabel
    , ucSendShortEmail

    -- * Delete Contact
    , deleteContact

    -- * Types
    , ContactType (..)

    , HasContact  (..)
    , Contact
    ) where

import Control.Lens
import Data.Aeson.Lens
import Data.Text               (Text)
import Network.HTTP.Types
import Network.PagerDuty.Internal.TH
import Network.PagerDuty.Internal.Types

default (Path)

contacts :: UserId -> Path
contacts u = "users" % u % "contact_methods"

data ContactType
    = SMS
    | Phone
    | Email
      deriving (Eq, Show)

deriveNullary ''ContactType

data Contact = Contact
    { _cId             :: ContactId
    , _cUserId         :: UserId
    , _cLabel          :: Text
    , _cAddress        :: Address
    , _cType           :: !ContactType
    , _cCountryCode    :: Maybe Int
    , _cSendShortEmail :: Maybe Bool'
    , _cBlacklisted    :: Maybe Bool'
    } deriving (Eq, Show)

deriveJSON ''Contact
makeClassy ''Contact

-- | List existing contact methods for the specified user.
--
-- @GET \/users\/\:user_id\/contact_methods@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/users/contact_methods/list>
listContacts :: UserId -> Request Empty s [Contact]
listContacts u = empty & path .~ contacts u

-- | Get details for a contact method.
--
-- @GET \/users\/\:user_id\/contact_methods\/\:id@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/users/contact_methods/show>
getContact :: UserId -> ContactId -> Request Empty s Contact
getContact u c = empty & path .~ contacts u % c

data CreateContact = CreateContact
    { _ccType'           :: !ContactType
    , _ccAddress'        :: Address
    , _ccCountryCode'    :: Maybe Int
    , _ccLabel'          :: Maybe Text
    , _ccSendShortEmail' :: Maybe Bool'
    } deriving (Eq, Show)

jsonRequest ''CreateContact

-- | The type of the contact method.
ccType :: Lens' (Request CreateContact s b) ContactType
ccType = upd.ccType'

-- | The id of the contact method. For 'SMS' and 'Phone' it is the number, and for
-- 'Email' it is the email address.
ccAddress :: Lens' (Request CreateContact s b) Address
ccAddress = upd.ccAddress'

-- | The number code for your country. Not used for 'Email'.
--
-- /Default:/ 1
ccCountryCode :: Lens' (Request CreateContact s b) (Maybe Int)
ccCountryCode = upd.ccCountryCode'

-- | A human friendly label for the contact method.
--
-- /Example:/ "Home Phone", "Work Email", etc.
--
-- /Default:/ The type of the contact method and the address (with country code
-- for phone numbers).
ccLabel :: Lens' (Request CreateContact s b) (Maybe Text)
ccLabel = upd.ccLabel'

-- | Send an abbreviated email message instead of the standard email
-- output. Useful for email-to-SMS gateways and email based pagers. Only valid for
-- 'Email' contact methods.
--
-- /Default:/ false.
ccSendShortEmail :: Lens' (Request CreateContact s b) (Maybe Bool)
ccSendShortEmail = upd.ccSendShortEmail'.mapping _B

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
createContact :: UserId
              -> ContactType
              -> Address
              -> Request CreateContact s Contact
createContact u t a =
    mk CreateContact
        { _ccType'           = t
        , _ccAddress'        = a
        , _ccCountryCode'    = Nothing
        , _ccLabel'          = Nothing
        , _ccSendShortEmail' = Nothing
        } & meth   .~ POST
          & path   .~ contacts u
          & unwrap .~ key "contact_method"

data UpdateContact = UpdateContact
    { _ucAddress'        :: Maybe Address
    , _ucCountryCode'    :: Maybe Int
    , _ucLabel'          :: Maybe Text
    , _ucSendShortEmail' :: Maybe Bool'
    } deriving (Eq, Show)

jsonRequest ''UpdateContact

-- | The id of the contact method. For 'SMS' and 'Phone' it is the number, and for
-- 'Email' it is the email address.
ucAddress :: Lens' (Request UpdateContact s b) (Maybe Address)
ucAddress = upd.ucAddress'

-- | The number code for your country. Not used for 'Email'.
--
-- /Default:/ 1
ucCountryCode :: Lens' (Request UpdateContact s b) (Maybe Int)
ucCountryCode = upd.ucCountryCode'

-- | A human friendly label for the contact method.
--
-- /Example:/ "Home Phone", "Work Email", etc.
--
-- /Default:/ The type of the contact method and the address (with country code
-- for phone numbers).
ucLabel :: Lens' (Request UpdateContact s b) (Maybe Text)
ucLabel = upd.ucLabel'

-- | Send an abbreviated email message instead of the standard email
-- output. Useful for email-to-SMS gateways and email based pagers. Only valid for
-- 'Email' contact methods.
--
-- /Default:/ false.
ucSendShortEmail :: Lens' (Request UpdateContact s b) (Maybe Bool)
ucSendShortEmail = upd.ucSendShortEmail'.mapping _B

-- | Update an existing contact method of a given user. Note that you cannot
-- change the type of an existing method.
--
-- @PUT \/users\/\:user_id\/contact_methods\/\:id@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/users/contact_methods/update>
updateContact :: UserId -> ContactId -> Request UpdateContact s Contact
updateContact u c =
    mk UpdateContact
        { _ucAddress'        = Nothing
        , _ucCountryCode'    = Nothing
        , _ucLabel'          = Nothing
        , _ucSendShortEmail' = Nothing
        } & meth   .~ PUT
          & path   .~ contacts u % c
          & unwrap .~ key "contact_method"

-- | Remove a contact method and any corresponding notification rules.
--
-- @DELETE \/users\/\:user_id\/contact_methods\/\:id@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/users/contact_methods/delete>
deleteContact :: UserId -> ContactId -> Request Empty s Empty
deleteContact u c = empty & meth .~ DELETE & path .~ contacts u % c
