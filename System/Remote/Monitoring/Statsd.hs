{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | This module lets you periodically flush metrics to a statsd
-- backend. Example usage:
--
-- > main = do
-- >     store <- newStore
-- >     forkStatsd defaultStatsdOptions store
--
-- You probably want to include some of the predefined metrics defined
-- in the ekg-core package, by calling e.g. the 'registerGcStats'
-- function defined in that package.
module System.Remote.Monitoring.Statsd
    (
      -- * The statsd syncer
      Statsd
    , statsdThreadId
    , forkStatsd
    , StatsdOptions(..)
    , defaultStatsdOptions
    ) where

import Control.Concurrent (ThreadId, myThreadId, threadDelay, throwTo)
import Control.Exception (IOException, catch)
import Control.Monad (forM_, when)
import qualified Data.ByteString.Char8 as B8
import qualified Data.HashMap.Strict as M
import Data.Int (Int64)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as Socket
import qualified System.Metrics as Metrics
import System.IO (stderr)

#if __GLASGOW_HASKELL__ >= 706
import Control.Concurrent (forkFinally)
#else
import Control.Concurrent (forkIO)
import Control.Exception (SomeException, mask, try)
import Prelude hiding (catch)
#endif

-- | A handle that can be used to control the statsd sync thread.
-- Created by 'forkStatsd'.
data Statsd = Statsd
    { threadId :: {-# UNPACK #-} !ThreadId
    }

-- | The thread ID of the statsd sync thread. You can stop the sync by
-- killing this thread (i.e. by throwing it an asynchronous
-- exception.)
statsdThreadId :: Statsd -> ThreadId
statsdThreadId = threadId

-- | Options to control how to connect to the statsd server and how
-- often to flush metrics. The flush interval should be shorter than
-- the flush interval statsd itself uses to flush data to its
-- backends.
data StatsdOptions = StatsdOptions
    { -- | Server hostname or IP address
      host :: !T.Text

      -- | Server port
    , port :: !Int

      -- | Data push interval, in ms.
    , flushInterval :: !Int

      -- | Print debug output to stderr.
    , debug :: !Bool

      -- | Prefix to add to all metric names.
    , prefix :: !T.Text

      -- | Suffix to add to all metric names. This is particularly
      -- useful for sending per host stats by settings this value to:
      -- @takeWhile (/= \'.\') \<$\> getHostName@, using @getHostName@
      -- from the @Network.BSD@ module in the network package.
    , suffix :: !T.Text
    }

-- | Defaults:
--
-- * @host@ = @\"127.0.0.1\"@
--
-- * @port@ = @8125@
--
-- * @flushInterval@ = @1000@
--
-- * @debug@ = @False@
defaultStatsdOptions :: StatsdOptions
defaultStatsdOptions = StatsdOptions
    { host          = "127.0.0.1"
    , port          = 8125
    , flushInterval = 1000
    , debug         = False
    , prefix        = ""
    , suffix        = ""
    }

-- | Create a thread that periodically flushes the metrics in the
-- store to statsd.
forkStatsd :: StatsdOptions  -- ^ Options
           -> Metrics.Store  -- ^ Metric store
           -> IO Statsd      -- ^ Statsd sync handle
forkStatsd opts store = do
    addrInfos <- Socket.getAddrInfo Nothing (Just $ T.unpack $ host opts)
                 (Just $ show $ port opts)
    socket <- case addrInfos of
        [] -> unsupportedAddressError
        (addrInfo:_) -> do
            socket <- Socket.socket (Socket.addrFamily addrInfo)
                      Socket.Datagram Socket.defaultProtocol
            Socket.connect socket (Socket.addrAddress addrInfo)
            return socket
    me <- myThreadId
    tid <- forkFinally (loop store emptySample socket opts) $ \ r -> do
        Socket.close socket
        case r of
            Left e  -> throwTo me e
            Right _ -> return ()
    return $ Statsd tid
  where
    unsupportedAddressError = ioError $ userError $
        "unsupported address: " ++ T.unpack (host opts)
    emptySample = M.empty

loop :: Metrics.Store   -- ^ Metric store
     -> Metrics.Sample  -- ^ Last sampled metrics
     -> Socket.Socket   -- ^ Connected socket
     -> StatsdOptions   -- ^ Options
     -> IO ()
loop store lastSample socket opts = do
    start <- time
    sample <- Metrics.sampleAll store
    let !diff = diffSamples lastSample sample
    flushSample diff socket opts
    end <- time
    threadDelay (flushInterval opts * 1000 - fromIntegral (end - start))
    loop store sample socket opts

-- | Microseconds since epoch.
time :: IO Int64
time = (round . (* 1000000.0) . toDouble) `fmap` getPOSIXTime
  where toDouble = realToFrac :: Real a => a -> Double

diffSamples :: Metrics.Sample -> Metrics.Sample -> Metrics.Sample
diffSamples prev curr = M.foldlWithKey' combine M.empty curr
  where
    combine m name new = case M.lookup name prev of
        Just old -> case diffMetric old new of
            Just val -> M.insert name val m
            Nothing  -> m
        _        -> M.insert name new m

    diffMetric :: Metrics.Value -> Metrics.Value -> Maybe Metrics.Value
    diffMetric (Metrics.Counter n1) (Metrics.Counter n2)
        | n1 == n2  = Nothing
        | otherwise = Just $! Metrics.Counter $ n2 - n1
    diffMetric (Metrics.Gauge n1) (Metrics.Gauge n2)
        | n1 == n2  = Nothing
        | otherwise = Just $ Metrics.Gauge n2
    diffMetric (Metrics.Label n1) (Metrics.Label n2)
        | n1 == n2  = Nothing
        | otherwise = Just $ Metrics.Label n2
    -- Distributions are assumed to be non-equal.
    diffMetric _ _  = Nothing

flushSample :: Metrics.Sample -> Socket.Socket -> StatsdOptions -> IO ()
flushSample sample socket opts = do
    forM_ (M.toList $ sample) $ \ (name, val) ->
        let fullName = dottedPrefix <> name <> dottedSuffix
        in  flushMetric fullName val
  where
    flushMetric name (Metrics.Counter n) = send "|c" name (show n)
    flushMetric name (Metrics.Gauge n)   = send "|g" name (show n)
    flushMetric _ _                      = return ()

    isDebug = debug opts
    dottedPrefix = if T.null (prefix opts) then "" else prefix opts <> "."
    dottedSuffix = if T.null (suffix opts) then "" else "." <> suffix opts
    send ty name val = do
        let !msg = B8.concat [T.encodeUtf8 name, ":", B8.pack val, ty]
        when isDebug $ B8.hPutStrLn stderr $ B8.concat [ "DEBUG: ", msg]
        Socket.sendAll socket msg `catch` \ (e :: IOException) -> do
            T.hPutStrLn stderr $ "ERROR: Couldn't send message: " <>
                T.pack (show e)
            return ()

------------------------------------------------------------------------
-- Backwards compatibility shims

#if __GLASGOW_HASKELL__ < 706
forkFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkFinally action and_then =
  mask $ \restore ->
    forkIO $ try (restore action) >>= and_then
#endif
