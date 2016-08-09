-- |
-- Description : Backend implementations for a BTRFS Docker volume plugin.
-- Stability   : experimental
module BtrfsVolume
  ( BtrfsProvider(..)
  ) where

import           Control.Exception (Exception(..), throwIO)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Data.Typeable (Typeable)
import           System.Directory (createDirectoryIfMissing, removeDirectoryRecursive, getDirectoryContents)
import           System.FilePath ((</>))

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

withPath :: FilePath -> (FilePath -> IO a) -> T.Text -> IO a
withPath root a name =
  case T.find (`elem` ("\NUL/" :: String)) name of
    Just _ -> throwIO (InvalidVolumeName name)
    Nothing -> a (root </> T.unpack name)

fakeProvider :: FilePath -> BtrfsProvider
fakeProvider root = BtrfsProvider
  { createVolume = withPath root (createDirectoryIfMissing False)
  , deleteVolume = withPath root removeDirectoryRecursive
  , listVolumes = map T.pack <$> listDirectory root
  }
