{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

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
    -- * List Users
      ListUsers
    , listUsers
    , luQuery

    -- * List On Call Users
    , listOnCallUsers

    -- * Create User
    , CreateUser
    , createUser
    , cuName
    , cuEmail
    , cuRole
    , cuJobTitle
    , cuTimeZone

    -- * Get User
    , getUser

    -- * Get User On Call
    , getUserOnCall

    -- * Update User
    , UpdateUser
    , updateUser
    , uuName
    , uuEmail
    , uuRole
    , uuJobTitle
    , uuTimeZone

    -- * Delete User
    , deleteUser

    -- * Types
    , PolicyInfo
    , piId
    , piName

    , OnCall
    , ocLevel
    , ocStart
    , ocEnd
    , ocEscalationPolicy

    , Role (..)

    , HasUserInfo (..)
    , UserInfo
    , User
    , uRole
    , uAvatarUrl
    , uUserUrl
    , uInvitationSent
    , uJobTitle
    ) where

import Control.Applicative     hiding (empty)
import Control.Lens            hiding ((.=))
import Data.Aeson
import Data.Default.Class
import Data.Monoid
import Data.Text               (Text)
import Data.Time
import Network.HTTP.Types
import Network.PagerDuty.Internal.TH
import Network.PagerDuty.Internal.Types

default (Path)

users :: Path
users = "users"

includes :: Query
includes =
    [ ("include[]", Just "contact_methods")
    , ("include[]", Just "notification_rules")
    ]

data UserInfo = UserInfo
    { _uId'       :: UserId
    , _uName'     :: Text
    , _uEmail'    :: Address
    , _uColor'    :: Text
    , _uTimeZone' :: TZ
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
    uTimeZone :: Lens' a TimeZone

    uId       = userInfo.uId'
    uName     = userInfo.uName'
    uEmail    = userInfo.uEmail'
    uColor    = userInfo.uColor'
    uTimeZone = userInfo.uTimeZone'._TZ

instance (QueryLike a, ToJSON a, HasUserInfo a)
    => HasUserInfo (Request a s b) where
        userInfo = upd.userInfo

instance HasUserInfo UserInfo where
    userInfo = id

data PolicyInfo = PolicyInfo
    { _piId   :: EscalationPolicyId
    , _piName :: Text
    } deriving (Eq, Show)

deriveRecord ''PolicyInfo

data OnCall = OnCall
    { _ocLevel            :: !Int
    , _ocStart            :: Maybe Date
    , _ocEnd              :: Maybe Date
    , _ocEscalationPolicy :: PolicyInfo
    } deriving (Eq, Show)

deriveRecord ''OnCall

data Role
    = RoleAdmin
    | RoleUser
    | RoleLimitedUser
      deriving (Eq, Show)

deriveNullaryWith (dropped 4 underscored) ''Role

instance Default Role where
    def = RoleUser

data User = User
    { _uInfo            :: UserInfo
    , _uRole            :: !Role
    , _uAvatarUrl       :: Text
    , _uUserUrl         :: Text
    , _uInvitationSent' :: !Bool'
    , _uJobTitle        :: Maybe Text
    } deriving (Eq, Show)

    -- FIXME: add notification_rules, contact_methods, on_call

makeLenses ''User

instance FromJSON User where
    parseJSON = withObject "user" $ \o ->
        User <$> parseJSON (Object o)
             <*> o .:  "role"
             <*> o .:  "avatar_url"
             <*> o .:  "user_url"
             <*> o .:  "invitation_sent"
             <*> o .:? "job_title"

instance ToJSON User where
    toJSON u = Object (x <> y)
      where
        Object x = toJSON (_uInfo u)
        Object y = object
            [ "role"            .= _uRole            u
            , "avatar_url"      .= _uAvatarUrl       u
            , "user_url"        .= _uUserUrl         u
            , "invitation_sent" .= _uInvitationSent' u
            , "job_title"       .= _uJobTitle        u
            ]

instance HasUserInfo User where
    userInfo = uInfo

uInvitationSent :: Lens' User Bool
uInvitationSent = uInvitationSent'._B

newtype ListUsers = ListUsers
    { _luQuery' :: Maybe Text
    }

queryRequest ''ListUsers

instance Paginate ListUsers

-- | Filters the result, showing only the users whose names or email addresses
-- match the query.
luQuery :: Lens' (Request ListUsers s b) (Maybe Text)
luQuery = upd.luQuery'

-- | List users of your PagerDuty account, optionally filtered by a search query.
--
-- @GET \/users@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/users/list>
listUsers :: Request ListUsers s [User]
listUsers =
    mk ListUsers
        { _luQuery' = Nothing
        } & path  .~ users
          & query .~ includes

-- | List all the existing escalation policies with currently on-call users.
--
-- If the start and end of an on-call object are null, then the user is always
-- on-call for an escalation policy level.
--
-- @GET \/escalation_policies\/on_call@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/escalation_policies/on_call>
listOnCallUsers :: Request ListUsers s [User]
listOnCallUsers = listUsers & path .~ users % "on_call"

-- | Get information about an existing user.
--
-- @GET \/users\/\:id@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/users/show>
getUser :: UserId -> Request Empty s User
getUser u = empty & path .~ users % u & query .~ includes

-- | Get a user object with that user's current on-call status. If the on-call
-- object is an empty list, the user is never on-call.
--
-- If the start and end of an on-call object are null, then the user is always
-- on-call for an escalation policy level.
--
-- @GET \/users\/\:id\/on_call@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/users/show_on_call>
getUserOnCall :: UserId -> Request Empty s User
getUserOnCall u = empty & path .~ users % u % "on_call" & query .~ includes

data CreateUser = CreateUser
    { _cuName'     :: Text
    , _cuEmail'    :: Address
    , _cuRole'     :: Maybe Role
    , _cuJobTitle' :: Maybe Text
    , _cuTimeZone' :: Maybe TZ
    } deriving (Eq, Show)

jsonRequest ''CreateUser

-- | The name of the user.
cuName :: Lens' (Request CreateUser s b) Text
cuName = upd.cuName'

-- | The email of the user. The newly created user will receive an email asking
-- to confirm the subscription.
cuEmail :: Lens' (Request CreateUser s b) Address
cuEmail = upd.cuEmail'

-- | The user's role.
cuRole :: Lens' (Request CreateUser s b) (Maybe Role)
cuRole = upd.cuRole'

-- | The job title of the user.
cuJobTitle :: Lens' (Request CreateUser s b) (Maybe Text)
cuJobTitle = upd.cuJobTitle'

-- | The time zone the user is in. If not specified, the time zone of the
-- account making the API call will be used.
cuTimeZone :: Lens' (Request CreateUser s b) (Maybe TimeZone)
cuTimeZone = upd.cuTimeZone'.mapping _TZ

-- | Create a new user for your account. An invite email will be sent asking
-- the user to choose a password.
--
-- @POST \/users@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/users/create>
createUser :: RequesterId
           -> Text    -- ^ 'cuName'
           -> Address -- ^ 'cuEmail'
           -> Request CreateUser s User
createUser r n e = auth (createUserBasic n e) & query .~ [("requester_id", r)]

-- | A version of 'disableService' which uses HTTP Basic authentication and
-- doesn't require a 'RequesterId'.
createUserBasic :: Text    -- ^ 'cuName'
                -> Address -- ^ 'cuEmail'
                -> Request CreateUser Basic User
createUserBasic n e =
    mk CreateUser
        { _cuName'     = n
        , _cuEmail'    = e
        , _cuRole'     = Nothing
        , _cuJobTitle' = Nothing
        , _cuTimeZone' = Nothing
        } & meth .~ POST
          & path .~ users

data UpdateUser = UpdateUser
    { _uuName'     :: Maybe Text
    , _uuEmail'    :: Maybe Address
    , _uuRole'     :: Maybe Role
    , _uuJobTitle' :: Maybe Text
    , _uuTimeZone' :: Maybe TZ
    } deriving (Eq, Show)

jsonRequest ''UpdateUser

-- | The name of the user.
uuName :: Lens' (Request UpdateUser s b) (Maybe Text)
uuName = upd.uuName'

-- | The email of the user. The newly created user will receive an email asking
-- to confirm the subscription.
uuEmail :: Lens' (Request UpdateUser s b) (Maybe Address)
uuEmail = upd.uuEmail'

-- | The user's role.
uuRole :: Lens' (Request UpdateUser s b) (Maybe Role)
uuRole = upd.uuRole'

-- | The job title of the user.
uuJobTitle :: Lens' (Request UpdateUser s b) (Maybe Text)
uuJobTitle = upd.uuJobTitle'

-- | The time zone the user is in. If not specified, the time zone of the
-- account making the API call will be used.
uuTimeZone :: Lens' (Request UpdateUser s b) (Maybe TimeZone)
uuTimeZone = upd.uuTimeZone'.mapping _TZ

-- | Update an existing user.
--
-- @PUT \/users\/\:id@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/users/update>
updateUser :: UserId -> Request UpdateUser s User
updateUser u =
    mk UpdateUser
        { _uuName'     = Nothing
        , _uuEmail'    = Nothing
        , _uuRole'     = Nothing
        , _uuJobTitle' = Nothing
        , _uuTimeZone' = Nothing
        } & meth .~ PUT
          & path .~ users % u

-- | Remove an existing user.
--
-- @DELETE \/users\/\:id@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/users/delete>
deleteUser :: UserId -> Request Empty s Empty
deleteUser u = empty & meth .~ DELETE & path .~ users % u
-- FIXME: deal with conflict errors
