{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE FlexibleInstances               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ViewPatterns               #-}

-- Module      : Network.PagerDuty.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.PagerDuty.Types where
    -- (
    -- -- * Requests
    --   Auth         (..)
    -- , Cred         (..)
    -- , Domain       (..)

    -- , Request      (..)
    -- , Request'

    -- , Paginate     (..)

    -- -- * Errors
    -- , Code         (..)
    -- , message
    -- , ServiceError (..)
    -- , Error        (..)

    -- -- * Primitives
    -- , Key          (..)
    -- , ServiceKey
    -- , IncidentKey

    -- , Id           (..)
    -- , ServiceId
    -- , RequesterId

    -- , Empty        (..)
    -- ) where

import           Control.Applicative
import           Control.Lens          hiding ((.=))
import           Data.Aeson            hiding (Error)
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.HashMap.Strict   (HashMap)
import qualified Data.HashMap.Strict   as Map
import           Data.Monoid
import           Data.String
import           Data.Text             (Text)
-- import qualified Network.HTTP.Client   as Client
import           Network.PagerDuty.TH
import           Network.HTTP.Types
import qualified           Data.Time as Time

v1 :: ByteString -> ByteString
v1 = mappend "/api/v1/"

newtype Date = Date { unDate :: Time.LocalTime }
    deriving (Eq, Ord, Show)

instance ToJSON Date where
    toJSON = undefined

newtype TimeZone = TimeZone { unTZ :: Time.TimeZone }
    deriving (Eq, Ord, Show)

instance ToJSON TimeZone where
    toJSON = undefined

data Security = Basic | Token | None
    deriving (Eq, Show)

data Auth (a :: Security) where
    AuthBasic :: ByteString -> ByteString -> Auth Basic
    AuthToken :: ByteString -> Auth Token
    AuthNone  :: Auth None

deriving instance Eq   (Auth a)
deriving instance Show (Auth a)

newtype SubDomain = SubDomain ByteString
    deriving (Eq, Show, IsString)

domain :: SubDomain -> ByteString
domain (SubDomain s)
    | base `BS.isSuffixOf` s = s
    | otherwise              = s <> base
  where
    base = ".pagerduty.com"

newtype Code = Code Integer
    deriving (Eq, Show)

deriveJSON ''Code

description :: Code -> Text
description (Code c) =
    case c of
        2000 -> "Internal Error"
        2001 -> "Invalid Input Provided"
        2002 -> "Arguments Caused Error"
        2003 -> "Missing Arguments"
        2004 -> "Invalid 'since' or 'until' Parameter Values"
        2005 -> "Invalid Query Date Range"
        2006 -> "Authentication Failed"
        2007 -> "Account Not Found"
        2008 -> "Account Locked"
        2009 -> "Only HTTPS Allowed For This Call"
        2010 -> "Access Denied"
        2011 -> "The action requires a 'requester_id' to be specified"
        2012 -> "Your account is expired and cannot use the API"
        _    -> "Unrecognised error code"

data ServiceError = ServiceError
    { _errCode    :: Code
    , _errMessage :: Text
    , _errErrors  :: [Text]
    } deriving (Eq, Show)

deriveJSON ''ServiceError
makeLenses ''ServiceError

data Error
    = Internal String
    | Error    ServiceError
      deriving (Eq, Show)

instance FromJSON Error where
    parseJSON = fmap Error . parseJSON

makePrisms ''Error

data Pager = Pager
    { _pgOffset :: !Int
      -- ^ The offset used in the execution of the query.
    , _pgLimit  :: !Int
      -- ^ The limit used in the execution of the query.
    , _pgTotal  :: !Int
      -- ^ The total number of records available.
    } deriving (Eq, Show)

makeLenses ''Pager

instance FromJSON a => FromJSON (a, Maybe Pager) where
    parseJSON = withObject "paginated" $ \o -> (,)
        <$> parseJSON (Object o)
        <*> optional  (pager o)
      where
        pager o = Pager
           -- The offset of the first record returned.
           -- Default is 0.
           <$> o .: "offset" .!= 0
           -- The number of records returned.
           -- Default (and max limit) is 100 for most APIs.
           <*> o .: "limit"  .!= 100
           <*> o .: "total"

instance ToJSON Pager where
    toJSON p = object
        [ "offset" .= _pgOffset p
        , "limit"  .= _pgLimit p
        ]

iterate :: Pager -> Pager -> Pager
iterate a b = a & pgTotal .~ 0 & pgOffset +~ _pgOffset b

data Request a (s :: Security) r where
    Request :: ToJSON a
            => { _rqPayload :: a
               , _rqMethod  :: !StdMethod
               , _rqPath    :: ByteString
               , _rqQuery   :: HashMap ByteString [ByteString]
               , _rqPager   :: Maybe Pager
               }
            -> Request a s r

instance ToJSON (Request a s r) where
    toJSON (Request a _ _ _ p) = Object $
        let Object x = toJSON a
         in case toJSON p of
                (Object y) -> x <> y
                _          -> x

req :: ToJSON a => StdMethod -> ByteString -> a -> Request a s r
req m p s = Request s m p mempty Nothing

upd :: ToJSON b => Lens (Request a s r) (Request b s r) a b
upd = lens _rqPayload (\(Request _ m p q g) x -> Request x m p q g)

rqMethod :: Lens' (Request a s r) StdMethod
rqMethod = lens _rqMethod (\s a -> s { _rqMethod = a })

rqPath :: Lens' (Request a s r) ByteString
rqPath = lens _rqPath (\s a -> s { _rqPath = a })

rqQuery :: Lens' (Request a s r) (HashMap ByteString [ByteString])
rqQuery = lens _rqQuery (\s a -> s { _rqQuery = a })

rqPager :: Lens' (Request a s r) (Maybe Pager)
rqPager = lens _rqPager (\s a -> s { _rqPager = a })

type Request' = Request ()

class Paginate a where
    next :: Functor f => Request a s r -> f Pager -> f (Request a s r)

data Service
data Incident
data Requester

newtype Key a = Key Text
    deriving (Eq, Show, IsString)

deriveJSON ''Key

type ServiceKey  = Key Service
type IncidentKey = Key Incident

newtype Id a = Id Text
    deriving (Eq, Show, IsString)

deriveJSON ''Id

type ServiceId   = Id Service
type RequesterId = Id Requester

data Empty = Empty

instance ToJSON Empty where
    toJSON = const (object [])

instance FromJSON Empty where
    parseJSON = withObject "empty" f
      where
        f !o | Map.null o = pure Empty
             | otherwise  = fail "Unexpected non-empty JSON object."

data Address
    = Email Text
    | Phone Text
      deriving (Eq, Show)

makePrisms ''Address

data User = User
    { _id :: PT23IWX,
    , _name :: Tim Wright,
    , _email :: tim@acme.com,
    , _time_zone :: Eastern Time (US & Canada),
    , _color :: purple,
    , _role :: owner,
    , _avatar_url :: https://secure.gravatar.com/avatar/923a2b907dc04244e9bb5576a42e70a7.png?d=mm&r=PG,
    , _user_url :: /users/PT23IWX,
    , _invitation_sent :: false,
    }

makeLenses ''User
