module Main where

import KubernetesProvisioner

main :: IO ()
main = do
  provisionEtcd
  provisionKubernetesMaster
--  exitCode <- detectDocker
--  correctPrerequisite exitCode "Could not detect docker"
