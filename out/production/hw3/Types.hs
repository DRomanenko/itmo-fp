module Types where

import Data.Time.Clock
import System.Directory (Permissions)

data FileInfo
  = FileInfo
  {
    fPath        :: FilePath
  , fPermissions :: Permissions
  , fType        :: String
  , fEditTime    :: UTCTime
  , fSize        :: Integer
  } deriving Eq

data DirInfo
  = DirInfo
  {
    dSize        :: Integer
  , dPath        :: FilePath
  , dQtyFiles    :: Int
  , dPermissions :: Permissions
  } deriving Eq

data Command
  = Help
  | Cd String
  | Ls String
  | Dir
  | CreateFolder String
  | CreateFile String
  | Cat String
  | Remove String
  | WriteFile String [String]
  | FindFile String
  | Information String
  | Exit
  deriving (Show, Eq)

class Monad m => FSActions m where
  cd                      :: FilePath -> m ()
  create_folder           :: FilePath -> m ()
  create_file             :: FilePath -> m ()
  remove                  :: FilePath -> m ()
  ls                      :: FilePath -> m [FilePath]
  lsWithoutSpecialEntries :: FilePath -> m [FilePath]
  cat                     :: FilePath -> m String
  appendToFile            :: FilePath -> [String] -> m ()
  find_file               :: FilePath -> m (Maybe String)
  getDirectoriesRecursive :: FilePath -> m [FilePath]
  dir_information         :: FilePath -> Permissions -> m DirInfo
  file_information        :: FilePath -> Permissions -> m FileInfo
  existsFile              :: FilePath -> m Bool
  existsDir               :: FilePath -> m Bool
  existsPath              :: FilePath -> m Bool
  toAbsPath               :: FilePath -> m FilePath
  getAllFilesInDir        :: FilePath -> m [FilePath]
  get_permissions         :: FilePath -> m Permissions
