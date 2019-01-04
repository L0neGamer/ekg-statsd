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
    , statsdFlush
    , statsdThreadId
    , forkStatsd
    , StatsdOptions(..)
    , defaultStatsdOptions
    ) where

import Control.Concurrent (ThreadId, myThreadId, threadDelay, throwTo)
import Control.Concurrent.MVar (modifyMVar_, newMVar)
import Control.Exception (IOException, AsyncException(ThreadKilled), catch, fromException)
import Control.Monad (foldM, when)
import qualified Data.ByteString.Char8 as B8
import qualified Data.HashMap.Strict as M
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as Socket
import qualified System.Metrics as Metrics
import qualified System.Metrics.Distribution.Internal as Distribution
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
    , flush    :: IO ()
    }

-- | The thread ID of the statsd sync thread. You can stop the sync by
-- killing this thread (i.e. by throwing it an asynchronous
-- exception.)
statsdThreadId :: Statsd -> ThreadId
statsdThreadId = threadId

-- | Flush a sample to the statsd server
--
-- @since 0.2.3.0
statsdFlush :: Statsd -> IO ()
statsdFlush = flush

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
    (sendSample, closeSocket) <- case addrInfos of
        [] -> unsupportedAddressError
        (addrInfo:_) -> do
            socket <- Socket.socket (Socket.addrFamily addrInfo)
                      Socket.Datagram Socket.defaultProtocol

            let socketAddress = Socket.addrAddress addrInfo

            sendSample <- if debug opts
              then do
                   Socket.connect socket socketAddress
                   return $ \msg -> Socket.sendAll   socket msg

              else return $ \msg -> Socket.sendAllTo socket msg socketAddress

            return (sendSample, Socket.close socket)

    priorCountsVar <- newMVar M.empty
    let flush = do
          sample <- Metrics.sampleAll store
          modifyMVar_ priorCountsVar (flushSample sample sendSample opts)

    me <- myThreadId
    tid <- forkFinally (loop opts flush) $ \ r -> do
        closeSocket
        case r of
            Left e  -> case fromException e of
              Just ThreadKilled  -> return ()
              _                  -> throwTo me e
            Right _ -> return ()

    return $ Statsd tid flush
  where
    unsupportedAddressError = ioError $ userError $
        "unsupported address: " ++ T.unpack (host opts)

loop :: StatsdOptions -- ^ Options
     -> IO ()         -- ^ Action to flush the sample
     -> IO ()
loop opts flush = do
    start <- time
    flush
    end <- time
    threadDelay (flushInterval opts * 1000 - fromIntegral (end - start))
    loop opts flush

-- | Microseconds since epoch.
time :: IO Int64
time = (round . (* 1000000.0) . toDouble) `fmap` getPOSIXTime
  where toDouble = realToFrac :: Real a => a -> Double

flushSample :: Metrics.Sample -> (B8.ByteString -> IO ()) -> StatsdOptions -> M.HashMap T.Text Int64 -> IO (M.HashMap T.Text Int64)
flushSample sample sendSample opts priorCounts =
    foldM flushOne priorCounts (M.toList sample)
  where
    flushOne pc (name, val) =
      let fullName = dottedPrefix <> name <> dottedSuffix
      in  flushMetric fullName val pc

    flushMetric name (Metrics.Counter n)      pc = sendCounter name n pc
    flushMetric name (Metrics.Gauge n)        pc = sendGauge name n >> return pc
    flushMetric name (Metrics.Distribution d) pc = sendDistribution name d pc
    flushMetric _    (Metrics.Label _)        pc = return pc

    sendGauge name n = send "|g" name (show n)

    -- The statsd convention is to send only the increment
    -- since the last report was made, not the total count.
    sendCounter name n pc = do
      let old = fromMaybe 0 (M.lookup name pc)
      send "|c" name (show (n - old))
      return (M.insert name n pc)

    sendDistribution name d pc = do
      sendGauge         (name <> "." <> "mean"    ) (Distribution.mean     d)
      sendGauge         (name <> "." <> "variance") (Distribution.variance d)
      uc <- sendCounter (name <> "." <> "count"   ) (Distribution.count    d) pc
      sendGauge         (name <> "." <> "sum"     ) (Distribution.sum      d)
      sendGauge         (name <> "." <> "min"     ) (Distribution.min      d)
      sendGauge         (name <> "." <> "max"     ) (Distribution.max      d)
      return uc

    isDebug = debug opts
    dottedPrefix = if T.null (prefix opts) then "" else prefix opts <> "."
    dottedSuffix = if T.null (suffix opts) then "" else "." <> suffix opts
    send ty name val = do
        let !msg = B8.concat [T.encodeUtf8 name, ":", B8.pack val, ty]
        when isDebug $ B8.hPutStrLn stderr $ B8.concat [ "DEBUG: ", msg]
        sendSample msg `catch` \ (e :: IOException) -> do
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
