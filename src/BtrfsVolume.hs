-- |
-- Description : Backend implementations for a BTRFS Docker volume plugin.
-- Stability   : experimental
module BtrfsVolume
  ( BtrfsProvider(..)
  , InvalidVolumeName(..)
  , plainDirectoryProvider
  , btrfsProvider
  , isValid
  , listDirectory
  ) where

import           Control.Exception (Exception(..), throwIO)
import           Control.Monad (unless)
import           Data.Maybe (isJust)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Data.Typeable (Typeable)
import           System.Directory (createDirectoryIfMissing, removeDirectoryRecursive, getDirectoryContents, doesDirectoryExist)
import           System.FilePath ((</>))
import           System.Linux.Btrfs (createSubvol, destroySubvol)

-- This is new in directory 1.2.5.0, copy-pasting the definition for now
listDirectory :: FilePath -> IO [FilePath]
listDirectory path =
  filter f <$> getDirectoryContents path
  where f filename = filename /= "." && filename /= ".."

data BtrfsProvider = BtrfsProvider
  { createVolume :: T.Text -> IO ()
  , deleteVolume :: T.Text -> IO ()
  , listVolumes :: IO [T.Text]
  }

data InvalidVolumeName = InvalidVolumeName T.Text
  deriving (Show, Eq, Typeable)

instance Exception InvalidVolumeName where
  displayException (InvalidVolumeName n) = "Invalid volume name: " <> show n


isValid :: T.Text -> Bool
isValid name
  | isJust (T.find (`elem` ("\NUL/" :: String)) name) = False
  | name == "." = False
  | name == ".." = False
  | T.null name = False
  | otherwise = True

withPath :: FilePath -> (FilePath -> IO a) -> T.Text -> IO a
withPath root a name
  | not (isValid name) = throwIO (InvalidVolumeName name)
  | otherwise = a (root </> T.unpack name)

plainDirectoryProvider :: FilePath -> BtrfsProvider
plainDirectoryProvider root = BtrfsProvider
  { createVolume = withPath root (createDirectoryIfMissing False)
  , deleteVolume = withPath root removeDirectoryRecursive
  , listVolumes = map T.pack <$> listDirectory root
  }

btrfsProvider :: FilePath -> BtrfsProvider
btrfsProvider root = BtrfsProvider
  { createVolume = withPath root $ \p -> do
      e <- doesDirectoryExist p
      unless e (createSubvol p)
  , deleteVolume = withPath root destroySubvol
  , listVolumes = map T.pack <$> listDirectory root
  }
