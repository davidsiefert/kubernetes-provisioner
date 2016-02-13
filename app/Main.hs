module Main where

import KubernetesProvisioner

main :: IO ()
main = do
  downloadKubernetes
  provisionEtcd
  exitCode <- detectDocker
  correctPrerequisite exitCode "Could not detect docker"
