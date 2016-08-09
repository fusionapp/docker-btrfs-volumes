-- |
-- Description : Types and instances for the Docker Volume plugin API.
-- Stability   : experimental
module Network.Docker.Plugin.VolumeDriver.Types
  (
  -- * Volume Plugin API
    VolumeAPI
  -- * Request types
  , Create(..)
  , Remove(..)
  , Mount(..)
  , Path(..)
  , Unmount(..)
  , GetVolume(..)
  -- * Response types
  , ErrorResponse(..)
  , MountpointResponse(..)
  , VolumeInfo(..)
  , VolumeResponse(..)
  , VolumesResponse(..)
  , VolumeScope(..)
  , Capabilities(..)
  -- * Common constructors and lenses
  , HasVolumeName(..)
  , HasCallerID(..)
  , HasErrorResponse(..)
  ) where

import Control.Lens (Lens')
import Data.Aeson (ToJSON(..), Value(String, Null), object, (.=))
import Data.Aeson.TH (deriveJSON, defaultOptions, Options(..))
import Data.Default (Default(..))
import Data.Map (Map)
import Data.Text (Text)
import Servant.API

import Network.Docker.Plugin.Types (DockerPluginV1)

-- Common fields / lenses

-- | A request type with a volume name.
class HasVolumeName t where
  -- | A 'Lens' into the volume name specified in a request.
  volumeName :: Lens' t Text

-- | A request type with a caller ID.
class HasCallerID t where
  -- | A 'Lens' into the caller ID specified in a request.
  volumeCaller :: Lens' t Text

-- | A response type which can have an error.
class HasErrorResponse t where
  -- | Construct a request from an error message.
  errorResponse :: Text -> t


-- Request types

-- | A @VolumeDriver.Create@ request.
data Create = Create
  { createName :: Text
  , createOpts :: Map Text Text
  } deriving (Show, Eq)

deriveJSON defaultOptions {fieldLabelModifier = drop 6} ''Create

instance HasVolumeName Create where
  volumeName f r = fmap (\n -> r {createName = n}) (f (createName r))
  {-# INLINE volumeName #-}


-- | A @VolumeDriver.Remove@ request.
data Remove = Remove
  { removeName :: Text
  } deriving (Show, Eq)

deriveJSON defaultOptions {fieldLabelModifier = drop 6} ''Remove

instance HasVolumeName Remove where
  volumeName f r = fmap (\n -> r {removeName = n}) (f (removeName r))
  {-# INLINE volumeName #-}


-- | A @VolumeDriver.Mount@ request.
data Mount = Mount
  { mountName :: Text
  , mountID :: Text
  } deriving (Show, Eq)

deriveJSON defaultOptions {fieldLabelModifier = drop 5} ''Mount

instance HasVolumeName Mount where
  volumeName f r = fmap (\n -> r {mountName = n}) (f (mountName r))
  {-# INLINE volumeName #-}

instance HasCallerID Mount where
  volumeCaller f r = fmap (\i -> r {mountID = i}) (f (mountID r))
  {-# INLINE volumeCaller #-}


-- | A @VolumeDriver.Path@ request.
data Path = Path
  { pathName :: Text
  } deriving (Show, Eq)

deriveJSON defaultOptions {fieldLabelModifier = drop 4} ''Path

instance HasVolumeName Path where
  volumeName f r = fmap (\n -> r {pathName = n}) (f (pathName r))
  {-# INLINE volumeName #-}


-- | A @VolumeDriver.Unmount@ request.
data Unmount = Unmount
  { unmountName :: Text
  , unmountID :: Text
  } deriving (Show, Eq)

deriveJSON defaultOptions {fieldLabelModifier = drop 7} ''Unmount

instance HasVolumeName Unmount where
  volumeName f r = fmap (\n -> r {unmountName = n}) (f (unmountName r))
  {-# INLINE volumeName #-}

instance HasCallerID Unmount where
  volumeCaller f r = fmap (\i -> r {unmountID = i}) (f (unmountID r))
  {-# INLINE volumeCaller #-}


-- | A @VolumeDriver.Get@ request.
data GetVolume = GetVolume
  { getName :: Text
  } deriving (Show, Eq)

deriveJSON defaultOptions {fieldLabelModifier = drop 3} ''GetVolume

instance HasVolumeName GetVolume where
  volumeName f r = fmap (\n -> r {getName = n}) (f (getName r))
  {-# INLINE volumeName #-}


-- Response types

-- | A response with nothing (for success), or an error.
data ErrorResponse = ErrorResponse (Maybe Text)
  deriving (Show, Eq)

instance ToJSON ErrorResponse where
  toJSON (ErrorResponse err) = object ["Err" .= err]

instance HasErrorResponse ErrorResponse where
  errorResponse m = ErrorResponse (Just m)
  {-# INLINE errorResponse #-}


-- | A response with a mountpoint, or an error.
data MountpointResponse = MountpointResponse Text
                        | MountpointErrorResponse Text
  deriving (Show, Eq)

instance ToJSON MountpointResponse where
  toJSON (MountpointResponse m) = object
                                  [ "Mountpoint" .= m
                                  , "Err" .= Null
                                  ]
  toJSON (MountpointErrorResponse m) = object ["Err" .= m]

instance HasErrorResponse MountpointResponse where
  errorResponse = MountpointErrorResponse


-- | Metadata for a volume.
data VolumeInfo = VolumeInfo
  { volumeInfoName :: Text
  , volumeInfoMountpoint :: Maybe Text
  -- , status :: ???
  } deriving (Show, Eq)

deriveJSON defaultOptions
  { fieldLabelModifier = drop 10
  , omitNothingFields = True
  }
  ''VolumeInfo

instance HasVolumeName VolumeInfo where
  volumeName f r = fmap (\n -> r {volumeInfoName = n}) (f (volumeInfoName r))


-- | A response with metadata about a volume, or an error.
data VolumeResponse = VolumeResponse VolumeInfo
                    | VolumeErrorResponse Text
  deriving (Show, Eq)

instance ToJSON VolumeResponse where
  toJSON (VolumeResponse vi) = object
                               [ "Err" .= Null
                               , "Volume" .= vi
                               ]
  toJSON (VolumeErrorResponse m) = object ["Err" .= m]

instance HasErrorResponse VolumeResponse where
  errorResponse = VolumeErrorResponse


-- | A response with metadata about multiple volumes, or an error.
data VolumesResponse = VolumesResponse [VolumeInfo]
                     | VolumesErrorResponse Text
  deriving (Show, Eq)

instance ToJSON VolumesResponse where
  toJSON (VolumesResponse vs) = object
                               [ "Err" .= Null
                               , "Volumes" .= vs
                               ]
  toJSON (VolumesErrorResponse m) = object ["Err" .= m]

instance HasErrorResponse VolumesResponse where
  errorResponse = VolumesErrorResponse


-- | Volume scope.
data VolumeScope = ScopeGlobal
                 -- ^ Global volumes are available across an entire cluster.
                 | ScopeLocal
                 -- ^ Local volumes are only available from the engine they're
                 -- created on.
  deriving (Show, Eq)

instance ToJSON VolumeScope where
  toJSON ScopeGlobal = String "global"
  toJSON ScopeLocal = String "local"

-- | Capabilities of this volume plugin.
data Capabilities = Capabilities
  { capScope :: VolumeScope
  } deriving (Show, Eq)

instance Default Capabilities where
  def = Capabilities { capScope = ScopeLocal }

instance ToJSON Capabilities where
  toJSON c =
    let co = object
             [ "Scope" .= capScope c
             ] in
      object ["Capabilities" .= co]            


-- API type

-- | The type of the Volume plugin API.
type VolumeAPI = "VolumeDriver.Create"
                 :> ReqBody '[JSON] Create
                 :> Post '[DockerPluginV1] ErrorResponse
            :<|> "VolumeDriver.Remove"
                 :> ReqBody '[JSON] Remove
                 :> Post '[DockerPluginV1] ErrorResponse
            :<|> "VolumeDriver.Mount"
                 :> ReqBody '[JSON] Mount
                 :> Post '[DockerPluginV1] MountpointResponse
            :<|> "VolumeDriver.Path"
                 :> ReqBody '[JSON] Path
                 :> Post '[DockerPluginV1] MountpointResponse
            :<|> "VolumeDriver.Unmount"
                 :> ReqBody '[JSON] Unmount
                 :> Post '[DockerPluginV1] ErrorResponse
            :<|> "VolumeDriver.Get"
                 :> ReqBody '[JSON] GetVolume
                 :> Post '[DockerPluginV1] VolumeResponse
            :<|> "VolumeDriver.List"
                 :> Post '[DockerPluginV1] VolumesResponse
            :<|> "VolumeDriver.Capabilities"
                 :> Post '[DockerPluginV1] Capabilities
