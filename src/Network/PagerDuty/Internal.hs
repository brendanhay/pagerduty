{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Network.PagerDuty.Internal
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.PagerDuty.Internal
    ( gToJson
    , gFromJson
    , jsonKey
    ) where

import Data.Aeson.Types
import Data.Char        (isUpper, toLower)
import GHC.Generics


gToJson :: (Generic a, GToJSON (Rep a)) => a -> Value
gToJson = genericToJSON defaultOptions { fieldLabelModifier = jsonKey }

gFromJson :: (Generic a, GFromJSON (Rep a)) => Value -> Parser a
gFromJson = genericParseJSON defaultOptions { fieldLabelModifier = jsonKey }

jsonKey :: String -> String
jsonKey str
    | head str == '_' = tail str
    | otherwise      = underscore str
  where
    underscore [] = []
    underscore (c : cs)
        | isUpper c = '_' : underscore (toLower c : cs)
        | otherwise =  c  : underscore cs
