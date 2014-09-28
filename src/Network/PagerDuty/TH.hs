-- Module      : Network.PagerDuty.TH
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.PagerDuty.TH
    (
    -- * Lenses
      makeLens

    -- * JSON
    , deriveJSON
    , deriveJSONWith
    -- ** Options
    , unprefixed
    , hyphenated
    , underscored
    ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Aeson.TH          as Aeson
import           Data.Aeson.Types
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Char8  as BS
import qualified Data.ByteString.Lazy   as LBS
import           Data.Char
import           Data.Conduit
import qualified Data.HashMap.Strict    as Map
import           Data.List
import           Data.Monoid
import           Data.String
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Network.HTTP.Types

makeLens k = makeLensesWith $
    lensRulesFor [(k, drop 1 k)] & simpleLenses .~ True

deriveJSON = deriveJSONWith underscored

deriveJSONWith o = Aeson.deriveJSON o

unprefixed :: Int -> Options -> Options
unprefixed n o = o
    { constructorTagModifier = constructorTagModifier o . drop n
    }

hyphenated :: Options
hyphenated = underscored
    { fieldLabelModifier     = hyphenate . unprefix
    , constructorTagModifier = hyphenate
    }

underscored :: Options
underscored = defaultOptions
    { fieldLabelModifier     = underscore . unprefix
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

unprefix :: String -> String
unprefix = dropWhile (not . isUpper)

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p = groupBy (const (not . p))
