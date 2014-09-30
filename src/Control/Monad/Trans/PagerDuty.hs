{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
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

module Control.Monad.Trans.PagerDuty where

import Control.Applicative
import Control.Lens
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Network.HTTP.Client         (Manager)
import Network.PagerDuty.Types

-- -- | Provides an alias for shortening type signatures if preferred.
-- --
-- -- Note: requires the @ConstraintKinds@ extension.
-- type MonadAWS m =
--     ( MonadBaseControl IO m
--     , MonadCatch m
--     , MonadResource m
--     , MonadError Error m
--     , MonadReader Env m
--     )

data Env (s :: Security) = Env
    { _envDomain  :: SubDomain
    , _envAuth    :: Auth s
    , _envManager :: Manager
    }

makeLenses ''Env

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
