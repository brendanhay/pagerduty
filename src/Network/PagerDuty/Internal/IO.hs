{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Module      : Network.PagerDuty.Internal.IO
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.PagerDuty.Internal.IO where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson                       hiding (Error)
import qualified Data.ByteString.Lazy             as LBS
import           Data.Monoid
import qualified Data.Text                        as Text
import           Network.HTTP.Client              (Manager, httpLbs)
import qualified Network.HTTP.Client              as Client
import           Network.HTTP.Types
import           Network.PagerDuty.Internal.Types

request :: (MonadIO m, ToJSON a, FromJSON b)
        => Manager
        -> Logger
        -> a
        -> Client.Request
        -> m (Either Error b)
request m l x rq = msg >> liftIO (httpLbs raw m) >>= response l
  where
    msg = debug l ("[Raw Request]\n" <> Text.pack (show raw))

    raw = rq
        { Client.secure         = True
        , Client.port           = 443
        , Client.requestHeaders = headers
        , Client.requestBody    = Client.RequestBodyLBS (encode x)
        }

    headers =
        [ ("Content-Type", "application/json")
        , ("Accept",       "application/json")
        ]

response :: forall m b. (MonadIO m, FromJSON b)
         => Logger
         -> Client.Response LBS.ByteString
         -> m (Either Error b)
response l rs = case statusCode (Client.responseStatus rs) of
    200 -> success
    201 -> success
    400 -> failure
    500 -> failure
    n   -> return . Left $ unhandled n
  where
    success = maybe (Left unknown) Right `liftM` parse
    failure = maybe (Left unknown) Left  `liftM` parse

    unhandled n = Internal $
        "PagerDuty returned unhandled status code: "
            <> Text.pack (show n)

    unknown = Internal $
        "Unable to parse response into a PagerDuty API compatible type: "
            <> Text.pack (show body)

    parse :: FromJSON a => m (Maybe a)
    parse = msg >> return (decode body)

    msg = debug l ("[Raw Response]\n" <> Text.pack (show rs))

    body = Client.responseBody rs
