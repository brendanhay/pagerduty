{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Module      : Network.PagerDuty.REST.Users
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Access and manipulate user data for your PagerDuty account. When a user is
-- shown inlined in other resources, a deleted user will have its @html_url@ attribute
-- set to null.
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/users>
module Network.PagerDuty.REST.Users
    (

    -- * Types
      HasUserInfo (..)
    , UserInfo
    , User
    , uRole
    , uAvatarUrl
    , uUrl
    , uInvitationSent
    ) where

import Control.Applicative     hiding (empty)
import Control.Lens            hiding ((.=))
import Data.Aeson
import Data.Monoid
import Data.Text               (Text)
import Data.Time
import Network.HTTP.Types
import Network.PagerDuty.TH
import Network.PagerDuty.Types

users :: Path
users = "users"

data UserInfo = UserInfo
    { _uId'       :: UserId
    , _uName'     :: Text
    , _uEmail'    :: Address
    , _uColor'    :: Text
    , _uTimeZone' :: Maybe TZ
    } deriving (Eq, Show)

deriveRecord ''UserInfo

class HasUserInfo a where
    userInfo  :: Lens' a UserInfo

    -- | The id of the user.
    uId       :: Lens' a UserId

    -- | The name of the user.
    uName     :: Lens' a Text

    -- | The user's email address.
    uEmail    :: Lens' a Address

    -- | The color used to represent the user in schedules.
    uColor    :: Lens' a Text

    -- | The user's personal time zone.
    uTimeZone :: Lens' a (Maybe TimeZone)

    uId       = userInfo.uId'
    uName     = userInfo.uName'
    uEmail    = userInfo.uEmail'
    uColor    = userInfo.uColor'
    uTimeZone = userInfo.uTimeZone'.mapping _TZ

instance (QueryLike a, ToJSON a, HasUserInfo a)
    => HasUserInfo (Request a s b) where
        userInfo = upd.userInfo

instance HasUserInfo UserInfo where
    userInfo = id

data User = User
    { _uInfo            :: UserInfo
    , _uRole            :: Text
    , _uAvatarUrl       :: Text
    , _uUrl             :: Text
    , _uInvitationSent' :: !Bool'
    } deriving (Eq, Show)

makeLenses ''User

instance FromJSON User where
    parseJSON = withObject "user" $ \o ->
        User <$> parseJSON (Object o)
             <*> o .: "role"
             <*> o .: "avatar_url"
             <*> o .: "url"
             <*> o .: "invitation_sent"

instance ToJSON User where
    toJSON u = Object (x <> y)
      where
        Object x = toJSON (_uInfo u)
        Object y = object
            [ "role"            .= _uRole u
            , "avatar_url"      .= _uAvatarUrl u
            , "url"             .= _uUrl u
            , "invitation_sent" .= _uInvitationSent' u
            ]

instance HasUserInfo User where
    userInfo = uInfo

uInvitationSent :: Lens' User Bool
uInvitationSent = uInvitationSent'._B

-- | List users of your PagerDuty account, optionally filtered by a search query.
--
-- @GET \/users@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/users/list>
listUsers = undefined

-- | Get information about an existing user.
--
-- @GET \/users\/\:id@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/users/show>
getUser = undefined

-- | Get a user object with that user's current on-call status. If the on-call
-- object is an empty list, the user is never on-call.
--
-- If the start and end of an on-call object are null, then the user is always
-- on-call for an escalation policy level.
--
-- @GET \/users\/\:id\/on_call@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/users/show_on_call>
getUserOnCall = undefined

-- | Create a new user for your account. An invite email will be sent asking
-- the user to choose a password.
--
-- @POST \/users@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/users/create>
createUser = undefined

-- | Update an existing user.
--
-- @PUT \/users\/\:id@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/users/update>
updateUser = undefined

-- | Remove an existing user.
--
-- @DELETE \/users\/\:id@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/users/delete>
deleteUser = undefined
