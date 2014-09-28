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
import           Data.Aeson             hiding (Error)
import qualified Data.Aeson.TH          as Aeson
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Char8  as BS
import qualified Data.ByteString.Lazy   as LBS
import           Data.Conduit
import qualified Data.HashMap.Strict    as Map
import           Data.Monoid
import           Data.String
import           Data.Text              (Text)
import           Network.HTTP.Types

deriveJSON = Aeson.deriveJSON Aeson.defaultOptions

-- makeLens k v = makeLensesWith options
--   where
--     options = lensRulesFor [(k , v)]
--         & simpleLenses       .~ True
--         & generateSignatures .~ False

      -- makeLens :: String
      --             -> String
      --             -> template-haskell:Language.Haskell.TH.Syntax.Name
      --             -> template-haskell:Language.Haskell.TH.Lib.DecsQ
