module Task6Tests
  ( task6
  ) where

import Lens.Micro ((^.))
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import Task6
import Test.Hspec (SpecWith, describe, it, shouldBe)

task6 :: SpecWith ()
task6 =
  describe "Testing Task6" $ do
    it "Testing getDirectory" $ do
      curDirectory <- getCurrentDirectory
      result <- getDirectory (curDirectory </> "test" </> "A")
      shouldBe result (Dir "A" [Dir "B" [File "C"]])
    it "Testing lenses - get name" $ do
      curDirectory <- getCurrentDirectory
      result <- getDirectory (curDirectory </> "test" </> "A")
      shouldBe (result^.name) ("A")
    it "Testing lenses - get contents" $ do
      curDirectory <- getCurrentDirectory
      result <- getDirectory (curDirectory </> "app")
      shouldBe (result^.contents) (_contents (Dir "app" [File "Main.hs"]))
