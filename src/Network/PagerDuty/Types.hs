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
    -- , RestError (..)
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
import           Control.Lens                 hiding ((.=))
import           Data.Aeson                   hiding (Error)
import           Data.Aeson.Types             (Parser)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Char8        as BS
import           Data.ByteString.Conversion
import           Data.Default
import qualified Data.HashMap.Strict          as Map
import           Data.Monoid
import           Data.String
import           Data.Text                    (Text)
import qualified Data.Text.Encoding           as Text
import           Data.Time
import           GHC.TypeLits
import           Network.HTTP.Types
import           Network.HTTP.Types.QueryLike
import           Network.PagerDuty.TH
import           System.Locale

-- FIXME: Verify IncidentId/IncidentKey .. *Id/*Key are actually different or needed

newtype Bool' = B Bool
    deriving (Eq, Show)

deriveJSON ''Bool'
makePrisms ''Bool'

instance QueryValueLike Bool' where
    toQueryValue (B True)  = Just "true"
    toQueryValue (B False) = Just "false"

newtype Date = Date { unDate :: UTCTime }
    deriving (Eq, Ord, Show)

makePrisms ''Date

instance FromJSON Date where
    parseJSON = fmap Date . parseJSON

instance ToJSON Date where
    toJSON = toJSON . unDate

instance ToByteString Date where
    builder = builder
        . formatTime defaultTimeLocale (iso8601DateFormat $ Just "%XZ")
        . unDate

instance QueryValueLike Date where
    toQueryValue = Just . toByteString'

newtype TZ = TZ { unTZ :: TimeZone }
    deriving (Eq, Show)

makePrisms ''TZ

instance FromJSON TZ where
    parseJSON = undefined

instance ToJSON TZ where
    toJSON = toJSON . Text.decodeUtf8 . toByteString'

instance ToByteString TZ where
    builder = builder . timeZoneName . unTZ

instance QueryValueLike TZ where
    toQueryValue = Just . toByteString'

instance Default TZ where
    def = TZ utc

data Security = Basic | Token
    deriving (Eq, Show)

data Auth (a :: Security) where
    AuthBasic :: ByteString -> ByteString -> Auth Basic
    AuthToken :: ByteString -> Auth Token

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

data IntegrationError = IntegrationError
    { _ieStatus  :: Text
    , _ieMessage :: Text
    , _ieErrors  :: [Text]
    } deriving (Eq, Show)

deriveRecord ''IntegrationError

data RestError = RestError
    { _reCode    :: Code
    , _reMessage :: Text
    , _reErrors  :: [Text]
    } deriving (Eq, Show)

deriveRecord ''RestError

data Error
    = Internal    String
    | Integration IntegrationError
    | Rest        RestError
      deriving (Eq, Show)

instance FromJSON Error where
    parseJSON o = (Rest <$> parseJSON o)
       <|> (Integration <$> parseJSON o)

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

-- | A path segment.
data Path where
    Path :: Path
    Seg  :: ToByteString a => a -> Path

instance Monoid Path where
    mempty                  = Path
    mappend x Path          = x
    mappend Path y          = y
    mappend (Seg x) (Seg y) = Seg (builder x <> "/" <> builder y)

instance IsString Path where
    fromString = Seg

instance ToByteString Path where
    builder Path    = mempty
    builder (Seg x) = builder x

(%) :: ToByteString a => Path -> a -> Path
a % b = a <> Seg b

renderPath :: Path -> ByteString
renderPath = toByteString' . mappend v1
  where
    v1 :: Path
    v1 = "/api/v1"

data Request a (s :: Security) b where
    Request :: (QueryLike a, ToJSON a)
            => { _rqMeth   :: !StdMethod
               , _rqPath   :: Path
               , _rqQuery  :: Query
               , _rqBody   :: a
               , _rqPager  :: Maybe Pager
               , _rqUnwrap :: Value -> Parser Value
               }
            -> Request a s b

instance ToJSON (Request a s b) where
    -- Manually unwrapped to ensure GADT constraint holds.
    toJSON (Request _ _ _ b p _) = Object $
        let Object x = toJSON b
         in case toJSON p of
                (Object y) -> x <> y
                _          -> x

type Unwrap = Getting (First Value) Value Value

-- | Create a defaulted request from the payload type.
mk :: (QueryLike a, ToJSON a) => a -> Request a s b
mk x = Request GET mempty mempty x Nothing pure

empty :: Request Empty s r
empty = mk Empty

-- | Lens into the body of a request.
upd :: (QueryLike a, ToJSON a) => Lens' (Request a s b) a
upd = lens _rqBody (\(Request m p q _ g u) x -> Request m p q x g u)

-- | Drop the security constraint.
auth :: Request a s b -> Request a t b
auth (Request x m p q g u) = Request x m p q g u

meth :: Lens' (Request a s b) StdMethod
meth = lens _rqMeth (\r x -> r { _rqMeth = x })

path :: Lens' (Request a s b) Path
path = lens _rqPath (\r x -> r { _rqPath = x })

query :: QueryValueLike v
      => Lens (Request a s b) (Request a s b) Query [(ByteString, v)]
query = lens _rqQuery (\r x -> r { _rqQuery = toQuery x })

pager :: Lens' (Request a s b) (Maybe Pager)
pager = lens _rqPager (\r x -> r { _rqPager = x })

unwrap :: Setter (Request a s b) (Request a s b) (Value -> Parser Value) Unwrap
unwrap f r = f (_rqUnwrap r) <&> \k -> r { _rqUnwrap = g k }
  where
    g k x = maybe (fail "Failed to extract nested keys.") return (x ^? k)

-- | Primarily to obtain a constraint for the pagination function, as well as
-- the overrideable flexibility.
class Paginate a where
    next :: Request a s b -> Maybe Pager -> Maybe (Request a s b)
    next _  Nothing       = Nothing
    next rq (Just x)
        | x^.pgTotal == 0 = Nothing
        | otherwise       = Just (rq & pager ?~ (x & pgOffset +~ x^.pgTotal))

newtype Key (a :: Symbol) = Key Text
    deriving (Eq, Show, IsString)

instance FromJSON (Key a) where
    parseJSON = withText "key" (return . Key)

instance ToJSON (Key a) where
    toJSON (Key k) = toJSON k

instance ToByteString (Key a) where
    builder (Key k) = builder k

instance QueryValueLike (Key a) where
    toQueryValue = Just . toByteString'

type ServiceKey  = Key "service"
type IncidentKey = Key "incident"

newtype Id (a :: Symbol) = Id Text
    deriving (Eq, Show, IsString)

instance FromJSON (Id a) where
    parseJSON = withText "id" (return . Id)

instance ToJSON (Id a) where
    toJSON (Id i) = toJSON i

instance ToByteString (Id a) where
    builder (Id i) = builder i

instance QueryValueLike (Id a) where
    toQueryValue = Just . toByteString'

type AlertId       = Id "alert"
type EmailFilterId = Id "email-filter"
type LogEntryId    = Id "log-entry"
type PolicyId      = Id "policy"
type RequesterId   = Id "requester"
type RuleId        = Id "rule"
type ScheduleId    = Id "schedule"
type ServiceId     = Id "service"
type TargetId      = Id "target"
type UserId        = Id "user"
type VendorId      = Id "vendor"

data Empty = Empty

instance ToJSON Empty where
    toJSON = const (object [])

instance FromJSON Empty where
    parseJSON = withObject "empty" f
      where
        f !o | Map.null o = pure Empty
             | otherwise  = fail "Unexpected non-empty JSON object."

instance QueryLike Empty where
    toQuery = const []

newtype Address = Address Text
    deriving (Eq, Show)

deriveJSON ''Address
makePrisms ''Address

instance ToByteString Address where
    builder (Address a) = builder a

instance QueryValueLike Address where
    toQueryValue = Just . toByteString'

data User = User
    { _userId             :: UserId
    , _userName           :: Text
    , _userEmail          :: Address
    , _userTimeZone       :: TZ
    , _userColor          :: Text
    , _userRole           :: Text
    , _userAvatarUrl      :: Text
    , _userUrl            :: Text
    , _userInvitationSent :: Bool'
    } deriving (Eq, Show)

deriveRecord ''User
