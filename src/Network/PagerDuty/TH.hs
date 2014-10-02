{-# LANGUAGE TemplateHaskell #-}

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
    -- * Compound expressions
      deriveNullary
    , deriveNullaryWith
    , deriveRecord
    , deriveQuery
    , deriveBody

    -- * JSON
    , deriveJSON
    , deriveJSONWith

    -- * Lenses
    , makeLens

    -- * Re-exported generics
    , deriveGeneric

    -- * Re-exported options
    , dropped
    , hyphenated
    , underscored
    ) where

import           Control.Applicative
import           Control.Lens
import qualified Data.Aeson.TH                as Aeson
import           Data.Aeson.Types
import           Data.ByteString              (ByteString)
import qualified Data.Text.Encoding           as Text
import           Generics.SOP.TH
import           Language.Haskell.TH
import           Network.HTTP.Types.QueryLike
import           Network.PagerDuty.Query
import           Network.PagerDuty.Options

deriveNullary :: Name -> Q [Dec]
deriveNullary = deriveNullaryWith underscored

deriveNullaryWith :: Options -> Name -> Q [Dec]
deriveNullaryWith o n = concat <$> sequence
    [ deriveJSONWith o n
    , [d|instance ToQuery $(conT n) where queryValues = value . toJSON|]
    ]

deriveRecord :: Name -> Q [Dec]
deriveRecord n = concat <$> sequence
    [ deriveJSON n
    , makeLenses n
    ]

deriveBody :: Name -> Q [Dec]
deriveBody n = concat <$> sequence
    [ makeLenses n
    , deriveJSON n
    , [d|instance QueryLike $(conT n) where toQuery = const []|]
    ]

deriveQuery :: Name -> Q [Dec]
deriveQuery n = concat <$> sequence
    [ deriveGeneric n
    , makeLenses n
    , [d|instance ToJSON $(conT n) where toJSON = const (toJSON (object []))|]
    , [d|instance QueryLike $(conT n) where toQuery = gquery|]
    ]

deriveJSON :: Name -> Q [Dec]
deriveJSON = deriveJSONWith underscored

deriveJSONWith :: Options -> Name -> Q [Dec]
deriveJSONWith = Aeson.deriveJSON

makeLens :: String -> Name -> Q [Dec]
makeLens k = makeLensesWith
    $ lensRulesFor [(k, drop 1 k)]
    & simpleLenses .~ True

value :: Value -> [ByteString]
value (String t) = [Text.encodeUtf8 t]
value _          = []
