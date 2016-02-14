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
import System.Directory (getHomeDirectory, doesFileExist, createDirectoryIfMissing)
import Codec.Compression.GZip (decompress)
import Codec.Archive.Tar (read, unpack)

downloadKubernetes :: IO ()
downloadKubernetes = do
  homePath <- getHomeDirectory
  downloadDir <- createDownloadDirectory
  kubernetesArchivePath <- downloadKubernetesInto downloadDir
  extractRelease kubernetesArchivePath homePath

createDownloadDirectory :: IO String
createDownloadDirectory = do
  homePath <- getHomeDirectory
  let downloadDirectory = homePath </> ".downloads"
  createDirectoryIfMissing True downloadDirectory
  return downloadDirectory

provisionEtcd :: IO ()
provisionEtcd = do
  homePath <- getHomeDirectory
  downloadDir <- createDownloadDirectory
  etcdArchivePath <- downloadEtcdInto downloadDir
  extractRelease etcdArchivePath homePath
  -- template config /etc/default/etcd: ETCD_OPTS="-name infra -listen-client-urls http://0.0.0.0:4001 -advertise-client-urls http://127.0.0.1:4001"

downloadKubernetesInto :: FilePath -> IO String
downloadKubernetesInto = downloadInto "https://github.com/kubernetes/kubernetes/releases/download/v1.1.2/kubernetes.tar.gz" "kubernetes-1.1.2.tar.gz"

downloadEtcdInto :: FilePath -> IO String
downloadEtcdInto = downloadInto "https://github.com/coreos/etcd/releases/download/v2.0.12/etcd-v2.0.12-linux-amd64.tar.gz" "etcd-2.0.12.tar.gz"

downloadInto :: String -> String -> FilePath -> IO String
downloadInto url outputFilename tempPath = do
  archivePath <- return $ tempPath </> outputFilename
  downloadIt <- not <$> doesFileExist archivePath
  if downloadIt then download url archivePath else return () 
  return archivePath

download :: String -> FilePath -> IO ()
download url out = do
  response <- get url
  writeFile out (response ^. responseBody)

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
