-- | Tests for 'BtrfsVolume'.
module BtrfsVolumeSpec where

import           Control.Exception (bracket_, tryJust)
import           Control.Monad (guard)
import           Data.Foldable (for_)
import qualified Data.Set as S
import           Data.Text.Arbitrary ()
import           System.Directory (createDirectory, removeDirectoryRecursive, makeAbsolute)
import           System.FilePath ((</>))
import           System.IO.Error (isIllegalOperation)
import           System.IO.Temp (createTempDirectory)
import           System.Linux.Btrfs (getSubvol, destroySubvol)
import           Test.Hspec (Spec, describe, it, afterAll_, around_, shouldThrow, shouldReturn, runIO, anyIOException)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck ((==>))
import           Test.QuickCheck.Monadic (monadicIO, run)

import           BtrfsVolume (BtrfsProvider(..), InvalidVolumeName(..), plainDirectoryProvider, isValid, btrfsProvider, listDirectory)

spec :: Spec
spec = do
  providerSpec "plainDirectoryProvider" mkPlainProvider
  e <- runIO $ tryJust (guard . isIllegalOperation) (getSubvol ".")
  case e of
    Left _ -> return ()
    Right _ -> providerSpec "btrfsProvider" mkBtrfsProvider
  where mkPlainProvider = do
          containingDir <- createTempDirectory "." "btrfs-hspec"
          let root = containingDir </> "root"
              provider = plainDirectoryProvider root
              withProvider = bracket_
                (createDirectory root)
                (removeDirectoryRecursive root)
              cleanup = removeDirectoryRecursive containingDir
          return (provider, withProvider, cleanup)
        mkBtrfsProvider = do
          containingDir <- createTempDirectory "." "btrfs-hspec"
          let root = containingDir </> "root"
              provider = btrfsProvider root
              withProvider = bracket_
                (createDirectory root)
                (do ds <- listDirectory root
                    for_ ds $ \p -> do
                      d <- makeAbsolute (root </> p)
                      destroySubvol d
                    removeDirectoryRecursive root)
              cleanup = removeDirectoryRecursive containingDir
          return (provider, withProvider, cleanup)

providerSpec :: String -> IO (BtrfsProvider, IO () -> IO (), IO ()) -> Spec
providerSpec desc mkProvider = do
  (BtrfsProvider{..}, withProvider, cleanup) <- runIO mkProvider
  let isIVN name (InvalidVolumeName n) = name == n
      illegalExamples = ["illegal/name", "illegal\NULname", "", ".", ".."]
  describe desc . afterAll_ cleanup . around_ withProvider $ do
    describe "createVolume" $ do
      it "throws an exception when passed an illegal volume name" $
        for_ illegalExamples $ \name ->
          createVolume name `shouldThrow` isIVN name
      prop "creates a volume with a legal name successfully" $
        \name -> isValid name ==> monadicIO $
          run $ createVolume name
      prop "does nothing when a volume of the given name already exists" $
        \name -> isValid name ==> monadicIO $ do
          run $ createVolume name
          run $ createVolume name
    describe "deleteVolume" $ do
      it "throws an exception when passed an illegal volume name" $
        for_ illegalExamples $ \name ->
          deleteVolume name `shouldThrow` isIVN name
      prop "throws an exception when trying to delete a nonexistent volume" $
        \name -> isValid name ==> monadicIO . run $
          deleteVolume name `shouldThrow` anyIOException
      prop "deletes an existing volume successfully" $
        \name -> isValid name ==> monadicIO . run $ do
          createVolume name
          deleteVolume name
    describe "listVolumes" $ do
      it "returns an empty list when no volumes have been created yet" $
        listVolumes `shouldReturn` []
      prop "returns that volume after creating a volume" $
        \name -> isValid name ==> monadicIO . run $ do
          createVolume name
          listVolumes `shouldReturn` [name]
      prop "returns an empty list after creating a volume and then deleting it" $
        \name -> isValid name ==> monadicIO . run $ do
          createVolume name
          deleteVolume name
          listVolumes `shouldReturn` []
      prop "returns both volumes after creating two volumes" $
        \name1 name2 -> isValid name1
                       && isValid name2
                       && name1 /= name2 ==> monadicIO . run $ do
          createVolume name1
          createVolume name2
          S.fromList <$> listVolumes `shouldReturn` S.fromList [name1, name2]
      prop "returns the second volume after creating two volumes and deleting the first" $
        \name1 name2 -> isValid name1
                       && isValid name2
                       && name1 /= name2 ==> monadicIO . run $ do
          createVolume name1
          createVolume name2
          deleteVolume name1
          listVolumes `shouldReturn` [name2]
