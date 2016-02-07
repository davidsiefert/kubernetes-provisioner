{-# LANGUAGE OverloadedStrings #-}
module KubernetesProvisioner where

import Prelude hiding (writeFile)
import Turtle (procStrict, Text, empty, ExitCode(..))
import System.IO.Temp (withSystemTempDirectory)
import Data.Monoid ((<>))
import System.FilePath ((</>))
import Network.Wreq (get, responseBody)
import Control.Lens ((^.))
import Data.ByteString.Lazy (writeFile)

downloadKubernetes :: IO ()
downloadKubernetes = do
  withSystemTempDirectory "kubernetes" downloadReleaseInto
  where
    downloadReleaseInto :: FilePath -> IO ()
    downloadReleaseInto tempPath = do
      response <- get "https://github.com/kubernetes/kubernetes/releases/download/v1.1.2/kubernetes.tar.gz"
      writeFile (tempPath </> "kubernetes-1.1.2.tar.gz") (response ^. responseBody)

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
