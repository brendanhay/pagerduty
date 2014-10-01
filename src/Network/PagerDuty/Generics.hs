{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- Module      : Network.PagerDuty.Generics
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.PagerDuty.Generics
    ( gquery
    , gqueryWith
    ) where

import           Data.Aeson.Types
import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Char8        as BS
import           Generics.SOP
import           Network.HTTP.Types
import           Network.HTTP.Types.QueryLike
import           Network.PagerDuty.Options

gquery :: forall a. (Generic a, HasDatatypeInfo a, All2 QueryValueLike (Code a))
       => a
       -> Query
gquery = gqueryWith underscored

gqueryWith :: forall a. (Generic a, HasDatatypeInfo a, All2 QueryValueLike (Code a))
       => Options
       -> a
       -> Query
gqueryWith o a = case datatypeInfo (Proxy :: Proxy a) of
    ADT     _ _ cs -> go cs         (from a)
    Newtype _ _ c  -> go (c :* Nil) (from a)
  where
    go :: (All2 QueryValueLike xss, SingI xss)
       => NP ConstructorInfo xss
       -> SOP I xss
       -> Query
    go cs (SOP sop) = unI . hcollapse $ hcliftA2' p (gctor o) cs sop

gctor :: All QueryValueLike xs
      => Options
      -> ConstructorInfo xs
      -> NP I xs
      -> K Query xs
gctor o (Constructor n) args =
    K . hcollapse $ hcliftA p (K . (k,) . toQueryValue . unI) args
  where
    k = BS.pack $ (constructorTagModifier o) n

gctor o (Record _ ns) args =
    K . hcollapse $ hcliftA2 p (gfield o) ns args

gctor o (Infix n _ _) (x :* y :* Nil) =
    K [ (k, toQueryValue (unI x))
      , (k, toQueryValue (unI y))
      ]
  where
    k = BS.pack $ (constructorTagModifier o) n

gctor _ (Infix _ _ _) _ =
    error "Network.PagerDuty.Generics.inaccessible"

gfield :: QueryValueLike a
       => Options
       -> FieldInfo a
       -> I a
       -> K (ByteString, Maybe ByteString) a
gfield o (FieldInfo f) (I a) =
    K (BS.pack $ (fieldLabelModifier o) f, toQueryValue a)

p :: Proxy QueryValueLike
p = Proxy
