module Task7Tests
  ( task7
  ) where

import Lens.Micro ((^..), (^?))
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import Task6 (getDirectory)
import Task7
import Test.Hspec (SpecWith, describe, it, shouldBe)

task7 :: SpecWith ()
task7 =
  describe "Testing Task7" $ do
    it "Testing command 'cd' and 'file'" $ do
      curDirectory <- getCurrentDirectory
      result <- getDirectory (curDirectory </> "test")
      shouldBe (result ^? (cd "A" . cd "B" . file "C")) (Just "C")
    it "Testing command 'cd' and 'ls'" $ do
      curDirectory <- getCurrentDirectory
      result <- getDirectory (curDirectory </> "test")
      shouldBe (result ^.. (cd "A" . cd "B" . ls)) (["C"])
