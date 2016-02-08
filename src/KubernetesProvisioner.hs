{-# LANGUAGE OverloadedStrings #-}
module KubernetesProvisioner where

import Prelude hiding (writeFile, read, readFile)
import Turtle (procStrict, Text, empty, ExitCode(..))
import System.IO.Temp (withSystemTempDirectory)
import Data.Monoid ((<>))
import System.FilePath ((</>))
import Network.Wreq (get, responseBody)
import Control.Lens ((^.))
import Data.ByteString.Lazy (readFile, writeFile)
import System.Directory (getHomeDirectory)
import Codec.Compression.GZip (decompress)
import Codec.Archive.Tar (read, unpack)

downloadKubernetes :: IO ()
downloadKubernetes = do
  withSystemTempDirectory "kubernetes" (\tempPath -> do
    kubernetesArchivePath <- downloadReleaseInto tempPath
    homePath <- getHomeDirectory
    extractRelease kubernetesArchivePath homePath
    )

downloadReleaseInto :: FilePath -> IO String
downloadReleaseInto tempPath = do
  response <- get "https://github.com/kubernetes/kubernetes/releases/download/v1.1.2/kubernetes.tar.gz"
  let archivePath = tempPath </> "kubernetes-1.1.2.tar.gz"
  writeFile archivePath (response ^. responseBody)
  return archivePath

extractRelease :: FilePath -> FilePath -> IO ()
extractRelease srcFile destDir = do
  unpack destDir . read . decompress =<< readFile srcFile

correctPrerequisite :: Int -> String -> IO ()
correctPrerequisite exit msg = case exit of
  0 -> return ()
  _ -> putStrLn msg

detectDocker :: IO Int
detectDocker = detectCommand "docker"

detectCommand :: Text -> IO Int
detectCommand cmd = do
  (exitCode, output) <- procStrict cmd [] empty
  case exitCode of
    ExitSuccess -> return 0
    ExitFailure n -> return n
