{-# LANGUAGE BangPatterns               #-}
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
import           Control.Lens
import           Data.Aeson                    hiding (Error)
import           Data.ByteString               (ByteString)
import qualified Data.ByteString.Char8         as BS
import qualified Data.HashMap.Strict           as Map
import           Data.Monoid
import           Data.String
import           Data.Text                     (Text)
import qualified Network.HTTP.Client           as Client
import           Network.PagerDuty.Internal.TH

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

data Request a (s :: Security) b where
    Request :: ToJSON a => a -> Client.Request -> Request a s b

type Request' = Request ()

class Paginate a where
    next :: Request a s b -> b -> Maybe (Request a s b)

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
