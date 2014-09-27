{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns     #-}

-- Module      : Network.PagerDuty.Internal
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
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
import Data.List
import GHC.Generics

gToJson :: (Generic a, GToJSON (Rep a)) => String -> a -> Value
gToJson prefix = genericToJSON defaultOptions
    { fieldLabelModifier = jsonKey prefix
    , omitNothingFields  = True
    }

gFromJson :: (Generic a, GFromJSON (Rep a)) => String -> Value -> Parser a
gFromJson prefix = genericParseJSON defaultOptions
    { fieldLabelModifier = jsonKey prefix }

jsonKey :: String -> String -> String
jsonKey prefix str = case str of
    (stripPrefix prefix -> Just cs) -> underscore cs
    _                               -> underscore str
  where
    underscore [] = []
    underscore (!c : cs)
        | isUpper c = '_' : underscore (toLower c : cs)
        | otherwise =  c  : underscore cs
