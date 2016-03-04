{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module KubernetesProvisioner where

import Prelude hiding (writeFile, read, readFile)
import Turtle (procStrict, Text, empty, ExitCode(..))
import System.IO.Temp (withSystemTempDirectory)
import Data.Monoid ((<>))
import System.FilePath ((</>))
import Network.Wreq (get, responseBody)
import Control.Lens ((^.))
import qualified Data.ByteString.Lazy as DBL (readFile, writeFile)
import qualified Data.ByteString as DB (ByteString, writeFile)
import System.Directory (getHomeDirectory, doesFileExist, createDirectoryIfMissing, copyFile)
import Codec.Compression.GZip (decompress)
import Codec.Archive.Tar (read, unpack)
import Data.FileEmbed (embedDir)
import System.Posix.Files (setFileMode, ownerExecuteMode)
import System.Process (callProcess)

templatesDir :: [(FilePath, DB.ByteString)]
templatesDir = $(embedDir "templates")

provisionEtcd :: IO ()
provisionEtcd = do
  homePath <- getHomeDirectory
  downloadDir <- createDownloadDirectory
  etcdArchivePath <- downloadEtcdInto downloadDir
  extractRelease etcdArchivePath homePath
  provisionExecutable "etcd" (homePath </> "etcd-v2.0.12-linux-amd64/etcd") "/opt/bin/etcd" 

provisionKubernetesMaster :: IO ()
provisionKubernetesMaster = do
  homePath <- getHomeDirectory
  downloadDir <- createDownloadDirectory
  kubernetesArchivePath <- downloadKubernetesInto downloadDir
  extractRelease kubernetesArchivePath homePath
  extractRelease (homePath </> "kubernetes" </> "server" </> "kubernetes-server-linux-amd64.tar.gz") homePath
  provisionExecutable "kube-apiserver" (homePath </> "kubernetes" </> "server" </> "bin" </> "kube-apiserver") "/opt/bin/kube-apiserver"
  provisionExecutable "kube-scheduler" (homePath </> "kubernetes" </> "server" </> "bin" </> "kube-scheduler") "/opt/bin/kube-scheduler"
  provisionExecutable "kube-controller-manager" (homePath </> "kubernetes" </> "server" </> "bin" </> "kube-controller-manager") "/opt/bin/kube-controller-manager"

provisionExecutable :: String -> FilePath -> FilePath -> IO ()
provisionExecutable serviceName srcExecutable destExecutable = do
  --putStrLn $ "===> Provisioning " <> serviceName
  createDirectoryIfMissing True "/opt/bin"
  --putStrLn $ "===> Copying " <> srcExecutable
  copyFile srcExecutable destExecutable
  setFileMode destExecutable ownerExecuteMode
  configData <- return $ lookup (serviceName <> ".default") templatesDir
  serviceData <- return $ lookup (serviceName <> ".service") templatesDir
  --putStrLn $ show configData
  --putStrLn $ show serviceData
  case installSystemdService serviceName <$> configData <*> serviceData of
    Just x -> x
    Nothing -> (putStrLn $ "Failed installing " <> serviceName)
  --putStrLn $ "===> Starting " <> serviceName
  mapM_ (callProcess "/bin/systemctl") [["daemon-reload"], ["start", serviceName], ["status", serviceName]]

createDownloadDirectory :: IO String
createDownloadDirectory = do
  homePath <- getHomeDirectory
  let downloadDirectory = homePath </> ".downloads"
  createDirectoryIfMissing True downloadDirectory
  return downloadDirectory

installSystemdService :: String -> DB.ByteString -> DB.ByteString -> IO ()
installSystemdService name environmentConfigContents serviceConfigContents = do
  putStrLn ("===> Writing environment config for " <> name)
  DB.writeFile ("/etc/default/" <> name) environmentConfigContents
  putStrLn ("===> Writing service config for " <> name)
  DB.writeFile ("/lib/systemd/system/" <> name <> ".service") serviceConfigContents

downloadKubernetesInto :: FilePath -> IO String
downloadKubernetesInto = downloadInto "https://github.com/kubernetes/kubernetes/releases/download/v1.1.2/kubernetes.tar.gz" "kubernetes-1.1.2.tar.gz"

downloadEtcdInto :: FilePath -> IO String
downloadEtcdInto = downloadInto "https://github.com/coreos/etcd/releases/download/v2.0.12/etcd-v2.0.12-linux-amd64.tar.gz" "etcd-2.0.12.tar.gz"

downloadInto :: String -> String -> FilePath -> IO String
downloadInto url outputFilename tempPath = do
  archivePath <- return $ tempPath </> outputFilename
  downloadIt <- not <$> doesFileExist archivePath
  if downloadIt then putStrLn ("Downloading " <> url <> " to " <> archivePath) else putStrLn ("Skipping download, " <> archivePath <> " already exists.")
  if downloadIt then download url archivePath else return () 
  return archivePath

download :: String -> FilePath -> IO ()
download url out = do
  response <- get url
  DBL.writeFile out (response ^. responseBody)

extractRelease :: FilePath -> FilePath -> IO ()
extractRelease srcFile destDir = do
  unpack destDir . read . decompress =<< DBL.readFile srcFile

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
