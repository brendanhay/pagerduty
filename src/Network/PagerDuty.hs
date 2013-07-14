{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}

-- |
-- Module      : Network.PagerDuty
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.PagerDuty
    (
    -- * Monadic Operations
      PagerDuty
    , pagerDuty

    -- * Parameter Types
    , SubDomain(..)
    , Auth(..)

    -- * Integration API
    , ServiceKey(..)
    , IncidentKey(..)
    , Event(..)
    , integrate

    -- * Errors
    , Error(..)
    , Code(..)
    , message
    ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.CatchIO
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString.Base64     as Base64
import           Data.ByteString.Char8      (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Maybe
import           Data.Monoid
import           Data.String
import           Data.Text                  (Text)
import           GHC.Generics
import           Network.Http.Client
import           Network.PagerDuty.Internal
import           OpenSSL                    (withOpenSSL)
import           System.IO.Streams          (InputStream)
import qualified System.IO.Streams          as Streams

newtype SubDomain = SubDomain ByteString
    deriving (Eq, Show, IsString)

data Auth
    = Basic ByteString ByteString
    | Token ByteString
      deriving (Eq, Show)

newtype ServiceKey = ServiceKey Text
    deriving (Eq, Show, Generic, IsString)

instance ToJSON ServiceKey
instance FromJSON ServiceKey

newtype IncidentKey = IncidentKey Text
    deriving (Eq, Show, Generic, IsString)

instance ToJSON IncidentKey
instance FromJSON IncidentKey

data Event
    = Trigger !ServiceKey !(Maybe IncidentKey) !Text !(Maybe Object)
    | Acknowledge !ServiceKey !IncidentKey !(Maybe Text) !(Maybe Object)
    | Resolve !ServiceKey !IncidentKey !(Maybe Text) !(Maybe Object)
    deriving (Eq, Show)

instance ToJSON Event where
    toJSON (Trigger svc minc desc mdets) =
        jsonEvent "trigger" svc minc (Just desc) mdets
    toJSON (Acknowledge svc inc mdesc mdets) =
        jsonEvent "acknowledge" svc (Just inc) mdesc mdets
    toJSON (Resolve svc inc mdesc mdets) =
        jsonEvent "resolve" svc (Just inc) mdesc mdets

jsonEvent :: Text
          -> ServiceKey
          -> Maybe IncidentKey
          -> Maybe Text
          -> Maybe Object
          -> Value
jsonEvent typ skey ikey desc dets = object $ catMaybes
    [ Just $ "event_type"  .= typ
    , Just $ "service_key" .= skey
    , ("incident_key" .=) <$> ikey
    , ("description"  .=) <$> desc
    , ("details"      .=) <$> dets
    ]

newtype Code = Code Integer
    deriving (Eq, Show, Generic)

instance FromJSON Code
instance ToJSON Code

message :: Code -> Text
message (Code c) = case c of
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

data Error
    = Internal String
    | External
      { _message :: Text
      , _code    :: Code
      , _errors  :: [Text]
      }

$(deriveJSON jsonKey ''Error)

data Env = Env
    { _host :: ByteString
    , _auth :: ByteString
    }

newtype PagerDuty a = PagerDuty { unWrap :: ReaderT Env IO a }
    deriving (Applicative, Functor, Monad, MonadIO, MonadCatchIO, MonadPlus)

pagerDuty :: MonadIO m => SubDomain -> Auth -> PagerDuty a => m a
pagerDuty dom auth pd = liftIO $ runReaderT (unWrap pd) env
  where
    env = Env (address dom) (header auth)

    address (SubDomain sub) = sub <> ".pagerduty.com"

    header (Basic user pass) = "Basic " <> Base64.encode (user <> ":" <> pass)
    header (Token token)     = "Token token=" <> token

integrate :: Event -> PagerDuty (Either Error IncidentKey)
integrate = request POST url
  where
    url = "https://events.pagerduty.com/generic/2010-04-15/create_event.json"

--
-- Internal
--

request :: (ToJSON a, FromJSON b)
        => Method
        -> ByteString
        -> a
        -> PagerDuty (Either Error b)
request meth path body = PagerDuty $ do
    Env{..} <- ask
    liftIO . withOpenSSL $
        bracket (open _host) closeConnection (query _auth)
  where
    open host = do
        ctx <- baselineContextSSL
        openConnectionSSL ctx host 443

    query auth conn = do
        enc <- Streams.fromLazyByteString $ encode body
        req <- buildRequest $ do
            http meth path
            setAccept "application/json"
            setHeader "Authorization" auth
        sendRequest conn req $ inputStreamBody enc
        receiveResponse conn result

result :: FromJSON a => Response -> InputStream ByteString -> IO (Either Error a)
result res str = case getStatusCode res of
    200 -> success
    201 -> success
    400 -> failure
    500 -> failure
    n   -> return . Left . Internal $
        "PagerDuty returned unhandled status code: " ++ show n
  where
    success = maybe (Left unknown) Right <$> parse
    failure = maybe (Left unknown) Left <$> parse

    unknown = Internal
        "Unable to parse response into a PagerDuty API compatible type: <body>"

    parse :: FromJSON b => IO (Maybe b)
    parse = do
        bs <- BL.fromChunks <$> Streams.toList str
        return $ decode bs
