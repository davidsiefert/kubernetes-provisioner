module Main where

import KubernetesProvisioner

main :: IO ()
main = do
  downloadKubernetes
  exitCode <- detectDocker
  correctPrerequisite exitCode "Could not detect docker"
