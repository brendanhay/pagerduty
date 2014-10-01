{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}

-- Module      : Generics.SOP.Query
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Generics.SOP.Query
    ( gquery
    ) where

import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Char8        as BS
import           Generics.SOP
import           Network.HTTP.Types
import           Network.HTTP.Types.QueryLike

gquery :: forall a. (Generic a, HasDatatypeInfo a, All2 QueryValueLike (Code a))
       => a
       -> Query
gquery a = case datatypeInfo (Proxy :: Proxy a) of
    ADT     _ _ cs -> go cs         (from a)
    Newtype _ _ c  -> go (c :* Nil) (from a)
  where
    go :: (All2 QueryValueLike xss, SingI xss)
       => NP ConstructorInfo xss
       -> SOP I xss
       -> Query
    go cs (SOP sop) = unI . hcollapse $ hcliftA2' p gctor cs sop

gctor :: All QueryValueLike xs => ConstructorInfo xs -> NP I xs -> K Query xs
gctor (Constructor (BS.pack -> n)) args =
    K . hcollapse $ hcliftA p (K . (n,) . toQueryValue . unI) args

gctor (Record _ ns) args =
    K . hcollapse $ hcliftA2 p gfield ns args

gctor (Infix (BS.pack -> n) _ _) (x :* y :* Nil) =
    K [ (n, toQueryValue (unI x))
      , (n, toQueryValue (unI y))
      ]

gctor (Infix _ _ _) _ = error "inaccessible"

gfield :: QueryValueLike a
       => FieldInfo a
       -> I a
       -> K (ByteString, Maybe ByteString) a
gfield (FieldInfo f) (I a) = K (BS.pack f, toQueryValue a)

p :: Proxy QueryValueLike
p = Proxy
