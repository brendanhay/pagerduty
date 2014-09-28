-- Module      : Network.PagerDuty.TH
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.PagerDuty.TH where

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
    lensRulesFor [(k, underscored k)] & simpleLenses .~ True

deriveJSON   = Aeson.deriveJSON options
deriveToJSON = Aeson.deriveToJSON options

options :: Options
options = defaultOptions
    { fieldLabelModifier = underscored . unprefixed
    , omitNothingFields  = True
    }

underscored :: String -> String
underscored = intercalate "_" . map lowered . splitBy isUpper

lowered :: String -> String
lowered = map toLower

unprefixed :: String -> String
unprefixed = dropWhile (not . isUpper)

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p = groupBy (const (not . p))



-- makeLens k v = makeLensesWith options
--   where
--     options = lensRulesFor [(k , v)]
--         & simpleLenses       .~ True
--         & generateSignatures .~ False

      -- makeLens :: String
      --             -> String
      --             -> template-haskell:Language.Haskell.TH.Syntax.Name
      --             -> template-haskell:Language.Haskell.TH.Lib.DecsQ
