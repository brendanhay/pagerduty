-- | Re-exports of the supported PagerDuty APIs
module Network.PagerDuty.API
    ( -- * Supported APIs
      module Network.PagerDuty.API.Integration
    -- * Common Types
    , PagerDuty
    , SubDomain (..)
    , Auth      (..)
    , Code      (..)
    , Error     (..)
    )
where

import Network.PagerDuty.API.Integration
import Network.PagerDuty.Types
