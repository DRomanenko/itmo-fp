module Main
  ( main
  ) where

import Task1Tests(task1)
import Task3Tests(task3)
import Task4Tests(task4)
import Task5Tests(task5)
import Task6Tests(task6)
import Task7Tests(task7)
import Test.Hspec(hspec)

main :: IO ()
main =
  hspec $ do
    task1
    task3
    task4
    task5
    task6
    task7
