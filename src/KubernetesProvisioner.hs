{-# LANGUAGE OverloadedStrings #-}
module KubernetesProvisioner where

import Turtle

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
