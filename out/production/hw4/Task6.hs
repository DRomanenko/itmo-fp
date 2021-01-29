module Task6
    ( FS(..)
    , getDirectory
    , name
    , contents
    , fileName
    , dirName
    ) where

import Control.Exception (Exception, throwIO)
import Lens.Micro (Lens', Traversal', lens)
import System.Directory (doesDirectoryExist, doesFileExist, getPermissions, listDirectory, readable)
import System.FilePath (splitPath, takeFileName, (</>))

-- | Data type for FS exception
data FSException =
    PathDoesNotExists
  deriving (Show)

instance Exception FSException

-- | The data type represented by a simple filesystem tree
data FS
    = Dir
    { _name     :: FilePath
    , _contents :: [FS]
    }
    | File
    { _name     :: FilePath
    }
  deriving (Eq, Show)

-- | Implementation of a function which "scans" the given directory
-- and creates an object of FS type like the getDirectory' function,
-- if there is no directory, Exception PathDoesNotExists.
getDirectory :: FilePath -> IO FS
getDirectory path = do
  perms <- getPermissions path
  if not $ readable perms
    then return $ Dir (takeFileName path) []
  else do
    isFile <- doesFileExist path
    isDirectory <- doesDirectoryExist path
    if isFile
      then return $ File (takeFileName path)
    else if isDirectory
      then do
        content <- listDirectory path
        contentFS <- mapM (\fs -> getDirectory (path </> fs)) content
        return $ Dir (last . splitPath $ path) contentFS
      else throwIO PathDoesNotExists

-- | Implementation of lens for name.
name :: Lens' FS FilePath
name = lens _name (\fs path -> fs { _name = path })

-- | Implementation of lens for contents.
contents :: Traversal' FS [FS]
contents = dirName . lens _contents (\fs path -> fs { _contents = path })

-- | Implementation of prism for files.
fileName :: Traversal' FS FS
fileName f fs@(File _) = id <$> f fs
fileName _ file        = pure file

-- | Implementation of prism for directories.
dirName :: Traversal' FS FS
dirName f fs@(Dir _ _) = id <$> f fs
dirName _ dir          = pure dir
