{-

This module defines our application's monad and any application-specific
information it requires.

-}

module Application
  ( Application
  , applicationInitializer
  ) where

import           Snap.Extension
{- import           Snap.Extension.Heist.Impl-}
{- import           Snap.Extension.Timer.Impl-}
import           Snap.Extension.DB.MongoDB
import           Data.UString (u)

------------------------------------------------------------------------------
-- | 'Application' is our application's monad. It uses 'SnapExtend' from
-- 'Snap.Extension' to provide us with an extended 'MonadSnap' making use of
-- the Heist and Timer Snap extensions.
type Application = SnapExtend ApplicationState


------------------------------------------------------------------------------
-- | 'ApplicationState' is a record which contains the state needed by the Snap
-- extensions we're using.  We're using Heist so we can easily render Heist
-- templates, and Timer simply to illustrate the config loading differences
-- between development and production modes.
data ApplicationState = ApplicationState
    { mongoDBState    :: MongoDBState
    {- , timerState      :: TimerState-}
    }


------------------------------------------------------------------------------
instance HasMongoDBState ApplicationState where
    getMongoDBState     = mongoDBState
    setMongoDBState s a = a { mongoDBState = s }


------------------------------------------------------------------------------
{- instance HasTimerState ApplicationState where-}
{-     getTimerState     = timerState-}
{-     setTimerState s a = a { timerState = s }-}


------------------------------------------------------------------------------
-- | The 'Initializer' for ApplicationState. For more on 'Initializer's, see
-- the documentation from the snap package. Briefly, this is used to
-- generate the 'ApplicationState' needed for our application and will
-- automatically generate reload\/cleanup actions for us which we don't need
-- to worry about.
applicationInitializer :: Initializer ApplicationState
applicationInitializer = do
    mongoDB <- mongoDBInitializer (host "127.0.0.1") 1 (u "thehall")
    {- timer <- timerInitializer-}
    return $ ApplicationState mongoDB
