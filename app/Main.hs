module Main where

import KubernetesProvisioner

main :: IO ()
main = do
  exitCode <- detectDocker
  correctPrerequisite exitCode "Could not detect docker"
