module Task3Tests
  ( manyPuts
  , task3
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad.List (forM_)
import Criterion.Main (bench, bgroup, defaultMain, whnf)
import Task3
import Test.Hspec (SpecWith, describe, it, shouldBe)

manyPuts :: ConcurrentHashTable Int Int -> IO ()
manyPuts curHT = do
    forM_ [1..(10 ^ (6 :: Int))] $ \item -> do
        putCHT item item curHT

task3 :: SpecWith ()
task3 =
  describe "Testing Task3" $ do
    it "Testing newCHT" $ do
      curHT   <- newCHT
      result  <- sizeCHT curHT
      shouldBe result 0
    it "Testing getCHT (empty ConcurrentHashTable)" $ do
      curHT   <- newCHT :: IO (ConcurrentHashTable Int Int)
      result  <- getCHT 38 curHT
      shouldBe result Nothing
    it "Testing getCHT (Not an empty ConcurrentHashTable)" $ do
      curHT  <- newCHT :: IO (ConcurrentHashTable Int Int)
      putCHT 239 38 curHT
      putCHT 38 239 curHT
      result <- getCHT 239 curHT
      shouldBe result (Just 38)
    it "Testing putCHT (just one elem)" $ do
      curHT  <- newCHT :: IO (ConcurrentHashTable Int Int)
      putCHT 239 38 curHT
      result <- getCHT 239 curHT
      shouldBe result (Just 38)
    it "Testing putCHT (adding with size expansion)" $ do
      curHT <- newCHT :: IO (ConcurrentHashTable Int Int)
      putCHT 1 1 curHT
      putCHT 2 2 curHT
      putCHT 3 3 curHT
      putCHT 4 4 curHT
      putCHT 5 5 curHT
      putCHT 6 6 curHT
      putCHT 7 7 curHT
      putCHT 26 38 curHT
      result <- getCHT 26 curHT
      shouldBe result (Just 38)
    it "Testing sizeCHT" $ do
      curHT   <- newCHT :: IO (ConcurrentHashTable Int Int)
      putCHT 55 38 curHT
      result  <- sizeCHT curHT
      shouldBe result 1
    it "Testing performance putCHT" $ do
      curHT <- newCHT
      defaultMain
        [ bgroup "opers" [ bench "10^6 putCHT"  $ whnf manyPuts curHT ]
        ]
    it "Testing put/get to asynchronous exposure" $ do
      let indexes = [1..(10 ^ (3 :: Int))] :: [Int]
      curHT <- newCHT
      putStrLn "Sequence of execution of threads:"
      _threadId1 <- forkIO $ do
        forM_ indexes $ \i ->
          putCHT ("key" <> show (i + 1)) (10 * i) curHT
        putStrLn "thread1 finished"
      _threadId2 <- forkIO $ do
        forM_ (reverse indexes) $ \i
          -> getCHT ("key" <> show (i + 1)) curHT
        putStrLn "thread2 finished"
      threadDelay 10000
      size <- sizeCHT curHT
      shouldBe size 1000
