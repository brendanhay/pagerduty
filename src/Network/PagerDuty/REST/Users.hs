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
module Network.PagerDuty.REST.Users
    (

    -- * Types
      User
    , uId
    , uName
    , uEmail
    , uTimeZone
    , uColor
    , uRole
    , uAvatarUrl
    , uUrl
    , uInvitationSent
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

data User = User
    { _uId             :: UserId
    , _uName           :: Text
    , _uEmail          :: Address
    , _uTimeZone       :: TZ
    , _uColor          :: Text
    , _uRole           :: Text
    , _uAvatarUrl      :: Text
    , _uUrl            :: Text
    , _uInvitationSent :: Bool'
    } deriving (Eq, Show)

deriveJSON ''User

makeLens "_uId"        ''User
makeLens "_uName"      ''User
makeLens "_uEmail"     ''User
makeLens "_uColor"     ''User
makeLens "_uRole"      ''User
makeLens "_uAvatarUrl" ''User
makeLens "_uUrl"       ''User

uTimeZone :: Lens' User TimeZone
uTimeZone = lens _uTimeZone (\u x -> u { _uTimeZone = x }) . _TZ

uInvitationSent :: Lens' User Bool
uInvitationSent =
    lens _uInvitationSent (\u x -> u { _uInvitationSent = x }) . _B
