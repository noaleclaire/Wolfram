module Main where

import Lib
import Wolfram
import System.Environment
import System.Exit (exitWith, ExitCode (ExitSuccess, ExitFailure), exitSuccess)
import Control.Monad

main :: IO ()
main = do
    args <- getArgs
    let myConfig = join $ toConfig <$> (manageArgs args)
    case (errorArgs myConfig) of
        "invalid options | ex: (the ruleset is mandatory)" ->
            putStrLn "invalid options | ex: (the ruleset is mandatory)" >>
            exitWith (ExitFailure 84)
        " " -> return()
    createPyramid myConfig (initListCell "*")
    exitSuccess
