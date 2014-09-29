{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
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
import           Control.Lens               hiding ((.=))
import           Data.Aeson                 hiding (Error)
import           Data.Aeson.Types           (Parser)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as BS
import           Data.ByteString.Conversion
import           Data.Function
import qualified Data.HashMap.Strict        as Map
import           Data.Monoid
import           Data.String
import           Data.Text                  (Text)
import qualified Data.Time                  as Time
import           Data.Time                  hiding (TimeZone)
import           GHC.TypeLits
import           Network.HTTP.Types
import           Network.PagerDuty.JSON

-- FIXME: Query String parameters vs JSON bodies for GET

newtype Date = Date { unDate :: ZonedTime }
    deriving (Show)

instance Eq Date where
    (Date a) == (Date b) =
           on (==) zonedTimeToLocalTime a b
        || on (==) zonedTimeZone a b

instance FromJSON Date where
    parseJSON = fmap Date . parseJSON

instance ToJSON Date where
    toJSON = toJSON . unDate

newtype TimeZone = TimeZone { unTZ :: Time.TimeZone }
    deriving (Eq, Ord, Show)

instance FromJSON TimeZone where
    parseJSON = undefined

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
        <*> optional  (parse o)
      where
        parse o = Pager
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

data Request a (s :: Security) r where
    Request :: ToJSON a
            => { _rqPayload :: a
               , _rqMethod  :: !StdMethod
               , _rqPath    :: ByteString
               , _rqQuery   :: Query
               , _rqPager   :: Maybe Pager
               , _rqUnwrap  :: Value -> Parser Value
               }
            -> Request a s r

instance ToJSON (Request a s r) where
    -- Manually unwrapped to ensure GADT constraint holds.
    toJSON (Request p _ _ _ q _) = Object $
        let Object x = toJSON p
         in case toJSON q of
                (Object y) -> x <> y
                _          -> x

type Unwrap = Getting (First Value) Value Value

-- | Create a defaulted request from the payload type.
mk :: ToJSON a => (Request a s r -> Request a s r) -> a -> Request a s r
mk f x = Request x GET "/" mempty Nothing pure & f

-- | Modify the request state.
upd :: ToJSON a => Lens' (Request a s r) a
upd = lens _rqPayload (\(Request _ m p q g u) x -> Request x m p q g u)

method :: Lens' (Request a s r) StdMethod
method = lens _rqMethod (\r x -> r { _rqMethod = x })

path :: ToByteString b => Lens (Request a s r) (Request a s r) ByteString b
path f r = fmap (\x -> r { _rqPath = rm (toByteString' x) }) (f (_rqPath r))
  where
    rm x | BS.null x              = v1
         |  "/" `BS.isPrefixOf` x = v1 <> x
         | otherwise              = v1 <> "/" <> x

    v1 = "/api/v1"

query :: Lens' (Request a s r) Query
query = lens _rqQuery (\r x -> r { _rqQuery = x })

pager :: Lens' (Request a s r) (Maybe Pager)
pager = lens _rqPager (\r x -> r { _rqPager = x })

unwrap :: Lens (Request a s r) (Request a s r) (Value -> Parser Value) Unwrap
unwrap f r = fmap (\x -> r { _rqUnwrap = g x }) (f (_rqUnwrap r))
  where
    g h x = maybe (fail "Failed to extract nested keys.") return (x ^? h)

-- | Primarily to obtain a constraint for the pagination function, as well as
-- the overrideable flexibility.
class Paginate a where
    next :: Request a s r -> Maybe Pager -> Maybe (Request a s r)
    next _  Nothing       = Nothing
    next rq (Just x)
        | x^.pgTotal == 0 = Nothing
        | otherwise       = Just (rq & pager ?~ (x & pgOffset +~ x^.pgTotal))

newtype Key (a :: Symbol) = Key Text
    deriving (Eq, Show, IsString)

instance ToByteString (Key a) where
    builder (Key k) = builder k

instance FromJSON (Key a) where
    parseJSON = withText "key" (return . Key)

instance ToJSON (Key a) where
    toJSON (Key k) = toJSON k

type ServiceKey  = Key "service"
type IncidentKey = Key "incident"

newtype Id (a :: Symbol) = Id Text
    deriving (Eq, Show, IsString)

instance ToByteString (Id a) where
    builder (Id i) = builder i

instance FromJSON (Id a) where
    parseJSON = withText "id" (return . Id)

instance ToJSON (Id a) where
    toJSON (Id i) = toJSON i

type AlertId       = Id "alert"
type PolicyId      = Id "policy"
type RequesterId   = Id "requester"
type RuleId        = Id "rule"
type ScheduleId    = Id "schedule"
type ServiceId     = Id "service"
type TargetId      = Id "target"
type UserId        = Id "user"
type EmailFilterId = Id "email-filter"
type VendorId      = Id "vendor"

data Empty = Empty

instance ToJSON Empty where
    toJSON = const (object [])

instance FromJSON Empty where
    parseJSON = withObject "empty" f
      where
        f !o | Map.null o = pure Empty
             | otherwise  = fail "Unexpected non-empty JSON object."

newtype Email = Email Text
      deriving (Eq, Show, IsString)

deriveJSON ''Email

newtype Phone = Phone Text
      deriving (Eq, Show, IsString)

deriveJSON ''Phone

data Address
    = AddrEmail Email
    | AddrPhone Phone
      deriving (Eq, Show)

instance FromJSON Address where
    parseJSON o =
            AddrEmail <$> parseJSON o
        <|> AddrPhone <$> parseJSON o

instance ToJSON Address where
    toJSON (AddrEmail e) = toJSON e
    toJSON (AddrPhone p) = toJSON p

makePrisms ''Address

data User = User
    { _userId             :: UserId
    , _userName           :: Text
    , _userEmail          :: Email
    , _userTimeZone       :: TimeZone
    , _userColor          :: Text
    , _userRole           :: Text
    , _userAvatarUrl      :: Text
    , _userUrl            :: Text
    , _userInvitationSent :: Bool
    } deriving (Eq, Show)

deriveJSON ''User
makeLenses ''User
