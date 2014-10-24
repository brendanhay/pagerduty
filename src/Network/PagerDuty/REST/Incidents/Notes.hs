{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- Module      : Network.PagerDuty.REST.Incidents.Notes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The Incident Notes API allows you to add notes to a specified incident.
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/incidents/notes>
module Network.PagerDuty.REST.Incidents.Notes
    (
    -- * List Notes
      listNotes

    -- * Create Note
    , CreateNote
    , createNote
    , createNoteBasic
    , cnContent

    -- * Types
    , Note
    , nId
    , nCreatedAt
    , nUser
    , nContent
    ) where

import Control.Lens                 hiding ((.=))
import Data.Aeson
import Data.Text                    (Text)
import Data.Time
import Network.HTTP.Types
import Network.PagerDuty.REST.Users
import Network.PagerDuty.Internal.TH
import Network.PagerDuty.Internal.Types

default (Path)

notes :: IncidentId -> Path
notes i = "incidents" % i % "notes"

data Note = Note
    { _nId        :: NoteId
    , _nCreatedAt :: Date
    , _nUser      :: User
    , _nContent   :: Text
    } deriving (Eq, Show)

deriveJSON ''Note

makeLens "_nId"      ''Note
makeLens "_nUser"    ''Note
makeLens "_nContent" ''Note

nCreatedAt :: Lens' Note UTCTime
nCreatedAt = lens _nCreatedAt (\n x -> n { _nCreatedAt = x }) . _D

-- | List existing notes for the specified incident.
--
-- @GET \/incidents\/\:incident_id\/notes@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/incidents/notes/create>
listNotes :: IncidentId -> Request Empty s [Note]
listNotes i = empty & path .~ notes i

newtype CreateNote = CreateNote
    { _cnContent' :: Maybe Text
    } deriving (Eq, Show)

makeLenses ''CreateNote

instance ToJSON CreateNote where
    toJSON cn = object ["note" .= object ["content" .= _cnContent' cn]]

instance QueryLike CreateNote where
    toQuery = const []

-- | The note content.
cnContent :: Lens' (Request CreateNote s b) (Maybe Text)
cnContent = upd.cnContent'

-- | Create a new note for the specified incident.
--
-- @POST \/incidents\/\:incident_id\/notes@
--
-- /See:/ <http://developer.pagerduty.com/documentation/rest/incidents/notes/create>
createNote :: IncidentId -> RequesterId -> Request CreateNote s Note
createNote i r = auth (createNoteBasic i) & query .~ [("requester_id", r)]

-- | A version of 'createNote' which uses HTTP Basic authentication and
-- doesn't require a 'RequesterId'.
createNoteBasic :: IncidentId -> Request CreateNote s Note
createNoteBasic i =
    mk CreateNote
        { _cnContent' = Nothing
        } & meth .~ POST
          & path .~ notes i
