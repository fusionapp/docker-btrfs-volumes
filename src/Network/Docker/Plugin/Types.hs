-- |
-- Description : Types and instances for the whole Docker plugin API.
-- Stability   : experimental
module Network.Docker.Plugin.Types
  ( DockerPluginV1
  , PluginSubsystem(..)
  , PluginAPI
  ) where

import Data.Aeson (encode, ToJSON(..), Value(String))
import Data.Text (Text)
import Network.HTTP.Media ((//))
import Servant.API

-- | Phantom type for the version 1 plugin API content-type.
data DockerPluginV1

instance Accept DockerPluginV1 where
  contentType _ = "application" // "vnd.docker.plugins.v1+json"

instance ToJSON a => MimeRender DockerPluginV1 a where
  mimeRender _ = encode

-- | Docker plugin subsystems.
data PluginSubsystem = DockerAuthz
                     | DockerNetworkDriver
                     | DockerVolumeDriver
                     | DockerGeneric Text
                     deriving (Show, Eq)

instance ToJSON PluginSubsystem where
  toJSON DockerAuthz = String "authz"
  toJSON DockerNetworkDriver = String "NetworkDriver"
  toJSON DockerVolumeDriver = String "VolumeDriver"
  toJSON (DockerGeneric s) = String s

-- | API type for operations that all plugins must support.
type PluginAPI = "Plugin.Activate" :> Post '[DockerPluginV1] [PluginSubsystem]
