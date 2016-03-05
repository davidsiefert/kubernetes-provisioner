module Main where

import KubernetesProvisioner

main :: IO ()
main = do
  provisionEtcd
  provisionKubernetesMaster
  provisionKubernetesMinion
