{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module Snap.Client
  ( TimedOut (..),
    runHttp,
  )
where

import Control.Exception (AsyncException (ThreadKilled), catchJust)
import Control.Monad.IO.Class (liftIO)
import Snap.Core
  ( MonadSnap,
    Request,
    Response,
    getTimeoutModifier,
    runSnap,
  )

-- | Indicates an external request timed out.
--
-- @since 1.0.0
data TimedOut = TimedOut
  deriving stock
    ( -- | @since 1.0.0
      Show,
      -- | @since 1.0.0
      Eq
    )

-- | Sends an HTTP request. This will use the timeout settings of the parent
-- 'MonadSnap', which can be set as usual. If the external server responds
-- within the specified time, its 'Response' will be packaged into a 'Right' and
-- passed to the specified handler; otherwise, a 'Left' holding a 'TimedOut'
-- will be passed to the handler. The result of the handler produces the final
-- response.
--
-- No logging will be done for the external request - if you need to log, use
-- the handler, or the surrounding 'MonadSnap'.
--
-- If an exception is thrown in the process of servicing the request (that is,
-- /before/ it reaches the handler), it will be rethrown in the context of
-- 'runHttp'; thus, if you need to handle this, handle it around 'runHttp', not
-- in the handler being passed.
--
-- @since 1.0.0
runHttp ::
  (MonadSnap m) =>
  Request ->
  (Either TimedOut Response -> m Response) ->
  m Response
runHttp req handler = do
  timeoutMod <- getTimeoutModifier
  res <-
    liftIO
      . catchJust sieve (Right <$> go timeoutMod)
      $ \_ -> pure . Left $ TimedOut
  handler res
  where
    go :: ((Int -> Int) -> IO ()) -> IO Response
    go f = snd <$> runSnap (pure ()) (const . pure $ ()) f req
    sieve :: AsyncException -> Maybe ()
    sieve = \case
      ThreadKilled -> Just ()
      _ -> Nothing
