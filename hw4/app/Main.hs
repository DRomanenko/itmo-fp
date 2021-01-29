module Main where

import Task8 (gridAfter, genGrid, gridSize, showGrid)

main :: IO ()
main = do
  putStrLn "Task8:"
  putStrLn $ gridAfter 30 genGrid `showGrid` gridSize
