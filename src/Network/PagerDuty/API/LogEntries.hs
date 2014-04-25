{-# LANGUAGE DeriveGeneric #-}

module Network.PagerDuty.API.LogEntries
where

import Data.Aeson
import GHC.Generics
import Network.PagerDuty.Types


data LogEntries = LogEntries
    { lesTotal      :: !Int
    , leslogEntries :: [LogEntry]
    } deriving (Eq, Show, Generic)

instance FromJSON LogEntries
instance ToJSON   LogEntries

logEntries :: PagerDuty Authenticated (Either Error LogEntries)
logEntries = undefined
