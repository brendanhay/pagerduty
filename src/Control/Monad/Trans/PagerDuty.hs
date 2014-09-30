{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-- Module      : Control.Monad.Trans.PagerDuty
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Control.Monad.Trans.PagerDuty
    (
    -- * Transformer
      PDT
    -- ** Aliases
    , PD

    -- * Run
    , runAWST

    -- * Environment
    , Env
    , envDomain
    , envAuth
    , envManager
--    , envLogging

    -- * Integration API
    , submit

    -- * REST API
    , send
    , paginate
    ) where

import           Control.Applicative
import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.Morph
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import           Data.Aeson
import           Data.Conduit
import           Network.HTTP.Client           (Manager)
import           Network.PagerDuty.Integration (Event, Response)
import qualified Network.PagerDuty.Integration as Int
import qualified Network.PagerDuty.REST        as REST
import           Network.PagerDuty.Types

data Env (s :: Security) = Env
    { _envDomain  :: SubDomain
    , _envAuth    :: Auth s
    , _envManager :: Manager
--    , _envLogging ::
    }

-- | A convenient alias for 'PDT' 'IO'.
type PD s = PDT s IO

newtype PDT s m a = PDT
    { unPDT :: ReaderT (Env s) (ExceptT Error m) a
    } deriving
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadThrow
        , MonadCatch
        , MonadReader (Env s)
        , MonadError  Error
        )

instance MonadTrans (PDT s) where
    lift = PDT . lift . lift
    {-# INLINE lift #-}

instance MonadBase b m => MonadBase b (PDT s m) where
    liftBase = liftBaseDefault
    {-# INLINE liftBase #-}

instance MonadTransControl (PDT s) where
    newtype StT (PDT s) a = StTAWS
        { unStTAWS :: StT (ExceptT Error) (StT (ReaderT (Env s)) a)
        }

    liftWith f = PDT $
        liftWith $ \g ->
            liftWith $ \h ->
                f (liftM StTAWS . h . g . unPDT)
    {-# INLINE liftWith #-}

    restoreT = PDT . restoreT . restoreT . liftM unStTAWS
    {-# INLINE restoreT #-}

instance MonadBaseControl b m => MonadBaseControl b (PDT s m) where
    newtype StM (PDT s m) a = StMPDT { unStMPDT :: ComposeSt (PDT s) m a }

    liftBaseWith = defaultLiftBaseWith StMPDT
    {-# INLINE liftBaseWith #-}

    restoreM = defaultRestoreM unStMPDT
    {-# INLINE restoreM #-}

instance MFunctor (PDT s) where
    hoist nat m = PDT (ReaderT (ExceptT . nat . runPDT m))
    {-# INLINE hoist #-}

instance MMonad (PDT s) where
    embed f m = ask >>= f . runPDT m >>= either throwError return
    {-# INLINE embed #-}

runPDT :: PDT s m a -> Env s -> m (Either Error a)
runPDT (PDT k) = runExceptT . runReaderT k

hoistEither :: (MonadError Error m) => Either Error a -> m a
hoistEither = either throwError return

scoped :: MonadReader (Env s) m => (Env s -> m a) -> m a
scoped f = ask >>= f

submit :: ( MonadIO m
          , MonadReader (Env s) m
          , MonadError Error m
          , Event a
          )
       => a
       -> m Response
submit = submitCatch >=> hoistEither

submitCatch :: ( MonadIO m
               , MonadReader (Env s) m
               , Event a
               )
            => a
            -> m (Either Error Response)
submitCatch x = scoped $ \e ->
    Int.submit (_envManager e) x

send :: ( MonadIO m
        , MonadReader (Env s) m
        , MonadError Error m
        , FromJSON b
        )
     => Request a s b
     -> m b
send = sendCatch >=> hoistEither

sendCatch :: ( MonadIO m
             , MonadReader (Env s) m
             , FromJSON b
             )
          => Request a s b
          -> m (Either Error b)
sendCatch x = scoped $ \Env{..} ->
    REST.send _envAuth _envDomain _envManager x

paginate :: ( MonadIO m
            , MonadReader (Env s) m
            , MonadError Error m
            , Paginate a
            , FromJSON b
            )
         => Request a s b
         -> Source m b
paginate x = paginateCatch x $= awaitForever (hoistEither >=> yield)

paginateCatch :: ( MonadIO m
                 , MonadReader (Env s) m
                 , Paginate a
                 , FromJSON b
                 )
              => Request a s b
              -> Source m (Either Error b)
paginateCatch x = scoped $ \Env{..} ->
    REST.paginate _envAuth _envDomain _envManager x
