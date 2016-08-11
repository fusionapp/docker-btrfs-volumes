module Main where

import Data.Monoid ((<>))
import Network.Socket (Family(AF_UNIX), SocketType(Stream), SockAddr(SockAddrUnix), socket, bind, listen, maxListenQueue, close)
import Network.Wai.Handler.Warp (runSettingsSocket, defaultSettings)
import Options.Applicative (Parser, strOption, long, metavar, help, value, showDefault, execParser, info, helper, fullDesc, progDesc, header)

parser :: Parser (FilePath, FilePath)
parser = (,)
  <$> strOption
  ( long "socket"
    <> metavar "PATH"
    <> value "/run/docker/plugin/btrfs.sock"
    <> showDefault
    <> help "Socket path to serve the API at"
  )
  <*> strOption
  ( long "basedir"
    <> metavar "PATH"
    <> help "Base directory to create btrfs subvolumes in"
  )

main :: IO ()
main = do
  (socketPath, basedir) <- execParser opts
  sock <- socket AF_UNIX Stream 0
  bind sock (SockAddrUnix socketPath)
  listen sock maxListenQueue
  let app = undefined
  runSettingsSocket defaultSettings sock app
  close sock
  where opts = info (helper <*> parser)
          ( fullDesc
            <> progDesc "Docker BTRFS volume plugin"
            <> header "docker-btrfs-volumes - a Docker volume plugin"
          )
