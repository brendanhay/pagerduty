{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.PagerDuty.HTTP
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.PagerDuty.HTTP where

import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson               hiding (Error)
import qualified Data.ByteString.Lazy     as LBS
import           Network.HTTP.Client      (Manager, httpLbs)
import qualified Network.HTTP.Client      as Client
import qualified Network.HTTP.Client.Lens as Lens
import           Network.HTTP.Types
import           Network.PagerDuty.Types

request :: (MonadIO m, ToJSON a, FromJSON b)
        => Manager
        -> a
        -> Client.Request
        -> m (Either Error b)
request m x rq = liftIO (httpLbs raw m) >>= response
  where
    raw = rq
        & Lens.secure         .~ True
        & Lens.port           .~ 443
        & Lens.requestHeaders .~ headers
        & Lens.requestBody    .~ Client.RequestBodyLBS (encode x)

    headers =
        [ ("Content-Type", "application/json")
        , ("Accept",       "application/json")
        ]

response :: (MonadIO m, FromJSON b)
         => Client.Response LBS.ByteString
         -> m (Either Error b)
response rs = case statusCode (Client.responseStatus rs) of
    200 -> success
    201 -> success
    400 -> failure
    500 -> failure
    n   -> return . Left $ unhandled n
  where
    success = maybe (Left unknown) Right `liftM` parse
    failure = maybe (Left unknown) Left  `liftM` parse

    unhandled n = Internal
        $ "PagerDuty returned unhandled status code: " ++ show n

    unknown = Internal
        $ "Unable to parse response into a PagerDuty API compatible type: "
        ++ show body

    parse :: (Monad m, FromJSON a) => m (Maybe a)
    parse = return (decode body)

    body = Client.responseBody rs
