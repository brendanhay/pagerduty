{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

-- | The "Integration API"
--
-- <http://developer.pagerduty.com/documentation/integration/events>
--
module Network.PagerDuty.API.Integration
    ( ServiceKey  (..)
    , IncidentKey (..)
    , Event       (..)
    , submitEvent
    )
where

import Control.Applicative
import Data.Aeson
import Data.ByteString         (ByteString)
import Data.Hashable
import Data.Maybe              (catMaybes)
import Data.String             (IsString)
import Data.Text               (Text)
import GHC.Generics
import Network.HTTP.Types
import Network.PagerDuty.IO
import Network.PagerDuty.Types


newtype ServiceKey = ServiceKey Text
    deriving (Eq, Show, Generic, IsString, Hashable)

instance ToJSON ServiceKey
instance FromJSON ServiceKey

newtype IncidentKey = IncidentKey Text
    deriving (Eq, Show, Generic, IsString, Hashable)

instance ToJSON IncidentKey
instance FromJSON IncidentKey

data Event
    = Trigger     !ServiceKey !(Maybe IncidentKey) !Text !(Maybe Object)
    | Acknowledge !ServiceKey !IncidentKey !(Maybe Text) !(Maybe Object)
    | Resolve     !ServiceKey !IncidentKey !(Maybe Text) !(Maybe Object)
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


eVENTS_URI :: ByteString
eVENTS_URI = "https://events.pagerduty.com/generic/2010-04-15/create_event.json"

submitEvent :: Event -> PagerDuty a (Either Error IncidentKey)
submitEvent = request methodPost eVENTS_URI
