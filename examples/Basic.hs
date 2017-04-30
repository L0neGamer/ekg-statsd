{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

import Control.Concurrent (threadDelay)
import Control.Exception (evaluate)
import Control.Monad (forever)
import Data.List (foldl')
import System.Metrics
import qualified System.Metrics.Counter as Counter
import System.Remote.Monitoring.Statsd

-- 'sum' is using a non-strict lazy fold and will blow the stack.
sum' :: Num a => [a] -> a
sum' = foldl' (+) 0

mean :: Fractional a => [a] -> a
mean xs = sum' xs / fromIntegral (length xs)

main :: IO ()
main = do
    store <- newStore
    registerGcMetrics store
    iters <- createCounter "iterations" store
    _ <- forkStatsd defaultStatsdOptions store
    let loop :: Int -> IO ()
        loop n = forever $ do
          let n' = fromIntegral n :: Double
          _ <- evaluate $ mean [1..n']
          Counter.inc iters
          threadDelay 2000
    loop 1000000
