{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
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

module Network.PagerDuty.Types
    (
    -- * Requests
      Auth         (..)
    , Cred         (..)
    , Domain       (..)

    , Request      (..)
    , Request'

    , Paginate     (..)

    -- * Errors
    , Code         (..)
    , message
    , ServiceError (..)
    , Error        (..)

    -- * Primitives
    , Key          (..)
    , ServiceKey
    , IncidentKey

    , Id           (..)
    , ServiceId
    , RequesterId

    , Empty        (..)
    ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson             hiding (Error)
import           Data.Aeson.TH
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Char8  as BS
import qualified Data.ByteString.Lazy   as LBS
import           Data.Conduit
import qualified Data.HashMap.Strict    as Map
import           Data.Monoid
import           Data.String
import           Data.Text              (Text)
import qualified Network.HTTP.Conduit   as Client
import           Network.HTTP.Conduit   hiding (Request, Response)
import           Network.HTTP.Types

-- makeLenses ''Client.Request

data Auth = Token | Basic
    deriving (Eq, Show)

data Cred (a :: Auth) where
    CredToken :: ByteString -> ByteString -> Cred Token
    CredBasic :: ByteString -> Cred Basic

deriving instance Eq   (Cred a)
deriving instance Show (Cred a)

newtype Domain = Domain ByteString
      deriving (Eq, Show, IsString)

newtype Code = Code Integer
    deriving (Eq, Show)

deriveJSON defaultOptions ''Code

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
    { _errMessage :: Text
    , _errCode    :: Code
    , _errErrors  :: [Text]
    } deriving (Eq, Show)

deriveJSON defaultOptions ''ServiceError

makeLenses ''ServiceError

data Error
    = Internal String
    | Error    ServiceError
      deriving (Eq, Show)

instance FromJSON Error where
    parseJSON = fmap Error . parseJSON

makePrisms ''Error

data Request s (a :: Auth) r where
    Request :: ToJSON s => s -> Client.Request -> Request s a r

type Request' = Request ()

class Paginate r where
    next :: Request s a r -> r -> Maybe (Request s a r)

data Service
data Incident
data Requester

newtype Key a = Key Text
    deriving (Eq, Show, IsString)

deriveJSON defaultOptions ''Key

type ServiceKey  = Key Service
type IncidentKey = Key Incident

newtype Id a = Id Text
    deriving (Eq, Show, IsString)

deriveJSON defaultOptions ''Id

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
