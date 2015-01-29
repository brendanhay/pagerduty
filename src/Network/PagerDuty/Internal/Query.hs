{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- Module      : Network.PagerDuty.Internal.Query
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.PagerDuty.Internal.Query
    ( QueryValues (..)
    , gquery
    , gqueryWith
    ) where

import           Data.Aeson.Types
import           Data.Bifunctor
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as BS
import           Data.ByteString.Conversion
import           Data.Function              (on)
import           Data.List                  (groupBy)
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import qualified Data.Text.Encoding         as Text
import           Generics.SOP
import           Network.HTTP.Types
import           Network.PagerDuty.Internal.Options

class QueryValues a where
    queryValues :: a -> [ByteString]

    default queryValues :: ToByteString a => a -> [ByteString]
    queryValues = (:[]) . toByteString'

instance QueryValues a => QueryValues (Maybe a) where
    queryValues (Just x) = queryValues x
    queryValues Nothing  = []

instance QueryValues Text where
    queryValues x = [Text.encodeUtf8 x]

gquery :: forall a. (Generic a, HasDatatypeInfo a, All2 QueryValues (Code a))
       => a
       -> Query
gquery = gqueryWith underscored

gqueryWith :: forall a. (Generic a, HasDatatypeInfo a, All2 QueryValues (Code a))
       => Options
       -> a
       -> Query
gqueryWith o a = case datatypeInfo (Proxy :: Proxy a) of
    ADT     _ _ cs -> go cs         (from a)
    Newtype _ _ c  -> go (c :* Nil) (from a)
  where
    go :: (All2 QueryValues xss, SingI xss)
       => NP ConstructorInfo xss
       -> SOP I xss
       -> Query
    go cs (SOP sop) = group . unI . hcollapse $ hcliftA2' p (gctor o) cs sop

    group :: [(ByteString, ByteString)] -> Query
    group = concatMap f . groupBy ((==) `on` fst)
      where
        f []           = []
        f [(k, v)]     = [(k, Just v)]
        f xs@((k,_):_) = let n = k <> "[]" in map (bimap (const n) Just) xs

gctor :: All QueryValues xs
      => Options
      -> ConstructorInfo xs
      -> NP I xs
      -> K [(ByteString, ByteString)] xs
gctor o (Constructor n) args =
    K . concat . hcollapse $ hcliftA p (K . map (k,) . queryValues . unI) args
  where
    k = BS.pack (constructorTagModifier o n)

gctor o (Record _ ns) args =
    K . concat . hcollapse $ hcliftA2 p (gfield o) ns args

gctor o (Infix n _ _) (x :* y :* Nil) =
    K $ map (k,) (queryValues (unI x) ++ queryValues (unI y))
  where
    k = BS.pack (constructorTagModifier o n)

gctor _ Infix{} _ =
    error "Network.PagerDuty.Generics.inaccessible"

gfield :: QueryValues a
       => Options
       -> FieldInfo a
       -> I a
       -> K [(ByteString, ByteString)] a
gfield o (FieldInfo f) (I a) =
    K $ map (k,) (queryValues a)
  where
    k = BS.pack (fieldLabelModifier o f)

p :: Proxy QueryValues
p = Proxy
