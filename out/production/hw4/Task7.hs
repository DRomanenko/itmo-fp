{-# LANGUAGE Rank2Types #-}

module Task7
  ( cd
  , ls
  , file
  ) where

import Lens.Micro (Traversal', filtered, traversed, (^.))

import Task6 (FS (..), contents, name)

-- | Implementation of a function that checks the FS that it is a file.
isFile :: FS -> Bool
isFile (File _) = True
isFile _        = False

-- | Implementation of a function that checks the FS that it is a directory.
isDir :: FS -> Bool
isDir (Dir _ _) = True
isDir _         = False

-- | Implementation of a function that allows you to go to a subdirectory with a specified name.
cd :: FilePath -> Traversal' FS FS
cd path = contents . traversed . filtered(\fs -> isDir fs && fs^.name == path)

-- | Implementation of a function that allows you to get a list of directory names.
ls :: Traversal' FS FilePath
ls = contents . traversed . name

-- | Implementation of a function that allows to get the name of a specific File, if it exists.
file :: FilePath -> Traversal' FS String
file s = contents . traversed . filtered(\fs -> isFile fs && fs^.name == s) . name
