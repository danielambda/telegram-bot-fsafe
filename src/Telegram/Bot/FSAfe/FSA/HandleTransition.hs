{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Telegram.Bot.FSAfe.FSA.HandleTransition where

class HandleTransition t s s' | t s -> s' where
  handleTransition :: t -> s -> s'

instance {-# OVERLAPPABLE #-}
         (HandleTransition t s s', Applicative m) => HandleTransitionM t s s' m where
  handleTransitionM t s = pure $ handleTransition t s
  automaticallyHandleCallbackQueries = True

class HandleTransitionM t s s' m | t s -> s' where
  handleTransitionM :: t -> s -> m s'
  automaticallyHandleCallbackQueries :: Bool
  automaticallyHandleCallbackQueries = False

