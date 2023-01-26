module Main (main) where

import Spec qualified
import Test.Hspec.Formatters
import Test.Hspec.JUnit
import Test.Hspec.Runner

main :: IO ()
main = do
    -- setEnv "JUNIT_ENABLED" "1"
    -- setEnv "JUNIT_OUTPUT_DIRECTORY" "/tmp"
    -- setEnv "JUNIT_SUITE_NAME" "my-tests"

    hspecJUnit Spec.spec
