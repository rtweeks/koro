module Main where

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.UTF8 as BSU
import System.Environment (getArgs, lookupEnv)
import Text.Yaml.Reference (yaml)
import Lib


main :: IO ()
main = do
    (contentName, content) <- getContent
    let (output, _) = normalize (yaml contentName content False)
    putStrLn output

-- Environment variable KRUN_IS_NOT_FILE indicates the argument is the code to be parsed
argIsYaml :: IO Bool
argIsYaml = do
    evv <- lookupEnv "KRUN_IS_NOT_FILE"
    return $ case evv of
        Just "" -> False
        Nothing -> False
        otherwise -> True

getContent :: IO (String, BS.ByteString)
getContent  = do
    [input] <- getArgs
    inputIsYaml <- argIsYaml
    if inputIsYaml
        then return ("<immediate input>", BSU.fromString input)
        else do
            content <- BS.readFile input
            return (input, content)
