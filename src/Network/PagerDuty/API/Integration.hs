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
import Data.Maybe              (catMaybes)
import Data.Text               (Text)
import GHC.Generics
import Network.PagerDuty.IO
import Network.PagerDuty.Types


submitEvent :: Event -> PagerDuty a (Either Error IncidentKey)
submitEvent = fmap (fmap incident_key) . request defaultRequest
    { method = methodPost
    , host   = "events.pagerduty.com"
    , path   = "/generic/2010-04-15/create_event.json"
    }


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

data EventResponse = ER
    { status       :: !Text
    , incident_key :: !IncidentKey
    , message      :: !Text
    } deriving (Show, Generic)

instance FromJSON EventResponse
