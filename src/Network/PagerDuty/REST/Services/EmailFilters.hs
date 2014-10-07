{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

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
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/services/email_filters>
module Network.PagerDuty.REST.Services.EmailFilters
    (
    -- * Create Email Filter
      createEmailFilter

    -- * Update Email Filter
    , updateEmailFilter

    -- * Delete Email Filter
    , deleteEmailFilter

    -- * Types
    , MatchMode          (..)
    , HasEmailFilterInfo (..)
    , EmailFilterInfo
    , EmailFilter
    , efId
    ) where

import Control.Applicative     hiding (empty)
import Control.Lens            hiding ((.=))
import Data.Aeson
import Data.ByteString.Builder (Builder)
import Data.Default
import Data.Monoid
import Data.Text               (Text)
import Network.HTTP.Types
import Network.PagerDuty.TH
import Network.PagerDuty.Types

default (Builder)

filters :: ServiceId -> Path
filters s = "services" % s % "email_filters"

data MatchMode
    = Always
    | Match
    | NoMatch
      deriving (Eq, Show)

deriveNullaryWith hyphenated ''MatchMode

-- FIXME: Tighten up this type! Make the regex required for match/no-match and
-- encode the conditional invariants.
data EmailFilterInfo = EmailFilterInfo
    { _efSubjectMode'   :: Maybe MatchMode
    , _efSubjectRegex'   :: Maybe Text
    , _efBodyMode'       :: Maybe MatchMode
    , _efBodyRegex'      :: Maybe Text
    , _efFromEmailMode'  :: Maybe MatchMode
    , _efFromEmailRegex' :: Maybe Text
    } deriving (Eq, Show)

deriveRecord ''EmailFilterInfo

instance Default EmailFilterInfo where
    def = EmailFilterInfo Nothing Nothing Nothing Nothing Nothing Nothing

instance QueryLike EmailFilterInfo where
    toQuery = const []

class HasEmailFilterInfo a where
    emailFilterInfo  :: Lens' a EmailFilterInfo

    -- | One of always, match, no-match, which, respectively, means to not
    -- filter the email trigger by subject, filter it if the email subject
    -- matches the given regex, or filter if it doesn't match the given regex.
    --
    -- /Default:/ always.
    efSubjectMode    :: Lens' a (Maybe MatchMode)

    -- | The regex to be used when subject_mode is match or no-match.
    -- It is a required parameter on such cases.
    efSubjectRegex   :: Lens' a (Maybe Text)

    -- | One of always, match, no-match, which, respectively, means to not filter
    -- the email trigger by body, filter it if the body email matches the given
    -- regex, or filter if it doesn't match the given regex.
    --
    -- /Default:/ always.
    efBodyMode       :: Lens' a (Maybe MatchMode)

    -- | The regex to be used when body_mode is match or no-match.
    -- It is a required parameter on such cases.
    efBodyRegex      :: Lens' a (Maybe Text)

    -- | One of always, match, no-match, which, respectively, means to not filter
    -- the email trigger by its from address, filter it if the email from address
    -- matches the given regex, or filter if it doesn't match the given regex.
    --
    -- /Default:/ always.
    efFromEmailMode  :: Lens' a (Maybe MatchMode)

    -- | The regex to be used when from_email_mode is match or no-match.
    -- It is a required parameter on such cases.
    efFromEmailRegex :: Lens' a (Maybe Text)

    efSubjectMode    = emailFilterInfo.efSubjectMode'
    efSubjectRegex   = emailFilterInfo.efSubjectRegex'
    efBodyMode       = emailFilterInfo.efBodyMode'
    efBodyRegex      = emailFilterInfo.efBodyRegex'
    efFromEmailMode  = emailFilterInfo.efFromEmailMode'
    efFromEmailRegex = emailFilterInfo.efFromEmailRegex'

instance (QueryLike a, ToJSON a, HasEmailFilterInfo a)
    => HasEmailFilterInfo (Request a s b) where
        emailFilterInfo = upd.emailFilterInfo

instance HasEmailFilterInfo EmailFilterInfo where
    emailFilterInfo = id

data EmailFilter = EmailFilter
    { _efId   :: EmailFilterId
    , _efInfo :: EmailFilterInfo
    } deriving (Eq, Show)

instance FromJSON EmailFilter where
    parseJSON = withObject "email_filter" $ \o ->
        EmailFilter <$> parseJSON (Object o)
                    <*> o .: "id"

instance ToJSON EmailFilter where
    toJSON e = Object (x <> y)
      where
        Object x = toJSON (_efInfo e)
        Object y = object ["id" .= _efId e]

instance HasEmailFilterInfo EmailFilter where
    emailFilterInfo = lens _efInfo (\e x -> e { _efInfo = x })

-- | The email filter ID.
makeLens "_efId" ''EmailFilter

-- | Create a new Email Filter for the specified service.
--
-- @POST \/services\/\:service_id\/email_filters@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/services/email_filters/create>
createEmailFilter :: ServiceId -> Request EmailFilterInfo s EmailFilter
createEmailFilter s = mk def & meth .~ POST & path .~ filters s

-- | Update an existing Email Filter.
--
-- @PUT \/services\/\:service_id\/email_filters\/\:id@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/services/email_filters/update>
updateEmailFilter :: ServiceId
                  -> EmailFilterId
                  -> Request EmailFilterInfo s Empty
updateEmailFilter s e = mk def & meth .~ PUT & path .~ filters s % e

-- | Delete an existing Email Filter.
--
-- @DELETE \/services\/\:service_id\/email_filters\/\:id@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/services/email_filters/delete>
deleteEmailFilter :: ServiceId -> EmailFilterId -> Request Empty s Empty
deleteEmailFilter s e = empty & meth .~ DELETE & path .~ filters s % e
