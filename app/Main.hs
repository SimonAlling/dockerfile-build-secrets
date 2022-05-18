{-|
This program reads the content of a Dockerfile from standard input and prints a JSON object containing all build secrets it references and whether they are required or not.

If the Dockerfile cannot be parsed, this program exits with a non-zero exit code.
-}

module Main (main) where

import DockerfileBuildSecrets (jsonFromDockerfile)
import Language.Docker (errorBundlePretty, parseStdin)
import System.Exit (die)


main :: IO ()
main = parseStdin >>= either handleParseError handleParseSuccess
    where
        handleParseError = die . errorBundlePretty
        handleParseSuccess = putStr . jsonFromDockerfile
