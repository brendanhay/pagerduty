-- Module      : Network.PagerDuty.Internal.Options
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.PagerDuty.Internal.Options
    ( dropped
    , hyphenated
    , underscored
    ) where

import           Data.Aeson.Types
import           Data.Char
import           Data.List

dropped :: Int -> Options -> Options
dropped n o = o
    { constructorTagModifier = constructorTagModifier o . drop n
    }

hyphenated :: Options
hyphenated = underscored
    { fieldLabelModifier     = unsuffix . hyphenate . unprefix
    , constructorTagModifier = hyphenate
    }

underscored :: Options
underscored = defaultOptions
    { fieldLabelModifier     = unsuffix . underscore . unprefix
    , constructorTagModifier = underscore
    , omitNothingFields      = True
    , allNullaryToStringTag  = True
    }

hyphenate :: String -> String
hyphenate = intercalate "-" . map lower . splitBy isUpper

underscore :: String -> String
underscore = intercalate "_" . map lower . splitBy isUpper

lower :: String -> String
lower = map toLower

unsuffix :: String -> String
unsuffix = takeWhile (/= '\'')

unprefix :: String -> String
unprefix = dropWhile (not . isUpper)

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p = groupBy (const (not . p))
