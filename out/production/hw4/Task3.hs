{-# LANGUAGE BangPatterns #-}

module Task3
  ( ConcurrentHashTable(..)
  , newCHT
  , getCHT
  , putCHT
  , sizeCHT
  ) where

import Control.Concurrent.STM (STM, TVar, atomically, modifyTVar', newTVar, readTVar, writeTVar)
import Data.Foldable (forM_)
import Data.Hashable (Hashable (..))
import qualified Data.Map as M
import qualified Data.Vector as V

-- | Implementation of the Concurrent Hashtable datatype (O(n) - memory).
data ConcurrentHashTable k v =
  ConcurrentHashTable
    { qtyItems  :: TVar Int
    , hashTable :: TVar (V.Vector (TVar (M.Map k v)))
    }

-- | Implementation of a function that creates a new concurrent hashtable with size 100.
newCHT :: IO (ConcurrentHashTable k v)
newCHT = atomically $ do
  qtyItems' <- newTVar 0
  items <- V.replicateM 100 (newTVar M.empty)
  vecTVar <- newTVar items
  return $! ConcurrentHashTable qtyItems' vecTVar

-- | Implementation of a function that calculates a hash value using a key.
getValue :: Hashable k => ConcurrentHashTable k v -> k -> STM (TVar (M.Map k v))
getValue h k = do
  items <- readTVar $ hashTable h
  return $! items V.!(hash k `mod` (V.length items))

-- | Implementation of a function that searches by key value.
getCHT :: (Hashable k, Ord k) => k -> ConcurrentHashTable k v -> IO (Maybe v)
getCHT k h = do
  list <- atomically $ do
    b <- getValue h k
    readTVar b
  return $! M.lookup k list

-- | Implementation of a function that performs key and value insertion
putCHT :: (Hashable k, Ord k) => k -> v -> ConcurrentHashTable k v -> IO ()
putCHT !k !v curHashTable = atomically $ do
  items <- readTVar $ hashTable curHashTable
  qtyItems' <- readTVar $ qtyItems curHashTable
  if (qtyItems' * 2 > V.length items) then do
      valNewItems <- readTVar $ hashTable curHashTable
      ret         <- mapM readTVar $ V.toList valNewItems
      newItems    <- V.replicateM (2 * V.length items) (newTVar M.empty)
      writeTVar (hashTable curHashTable) newItems
      forM_ (concatMap M.toList ret) (\(k', v') -> do
        curItem    <- getValue curHashTable k'
        valCurItem <- readTVar curItem
        writeTVar curItem $! M.insert k' v' valCurItem)
  else return ()
  curItem    <- getValue curHashTable k
  valCurItem <- readTVar curItem
  writeTVar curItem $! M.insert k v valCurItem
  modifyTVar' (qtyItems curHashTable) (+1)

-- | Implementation of a function that allows to know
-- the size of concurrent hashtable at the moment.
sizeCHT :: ConcurrentHashTable k v -> IO Int
sizeCHT h = atomically $ readTVar $ qtyItems h
