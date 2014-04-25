{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Network.PagerDuty.Types
where

import Control.Monad.Reader
import Data.Aeson
import Data.ByteString            (ByteString)
import Data.String
import Data.Text                  (Text)
import GHC.Generics
import Network.HTTP.Client        (Manager)
import Network.PagerDuty.Internal


data Env a
    = Env     !Host !Manager
    | AuthEnv !Host !Auth   !Manager

type PagerDuty a b = ReaderT (Env a) IO b


newtype SubDomain = SubDomain { subDomain :: ByteString }
    deriving (Eq, Show, IsString)

data Auth
    = Basic ByteString ByteString
    | Token ByteString
    deriving (Eq, Show)

data Authenticated
data UnAuthenticated

type Host = ByteString

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
    deriving (Eq, Show, Generic)

instance ToJSON   Error where toJSON    = gToJson
instance FromJSON Error where parseJSON = gFromJson
