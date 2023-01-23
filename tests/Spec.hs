import Test.HUnit

import System.Exit (exitWith, exitSuccess, ExitCode (ExitFailure))

import Operators (operatorTestList)

main :: IO ()
main = runTestTT ( test [
    operatorTestList
  ]) >>= (\x -> if errors x + failures x == 0
    then  exitSuccess
    else exitWith (ExitFailure 84))