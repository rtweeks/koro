module Main where

import qualified Data.ByteString.Lazy as BS
import System.Environment (getArgs, lookupEnv)
import Text.Yaml.Reference (yaml)
import Lib

main :: IO ()
main = do
    [filePath] <- getArgs
    content <- BS.readFile filePath
    let (output, _) = normalize (yaml filePath content False)
    putStr output
