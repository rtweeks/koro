module Main where

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.UTF8 as BSU
import qualified Data.Map.Strict as M
import System.Environment (getArgs, lookupEnv)
import Text.Yaml.Reference (yaml)
import Lib

data RunMode    = ParseYaml
                | EncodePlain

data Invocation = Invocation
    { iMode :: RunMode
    , iMainArg :: Maybe String
    , iImmediate :: Bool
    }

-- Environment Variables
krun_is_not_file = "KRUN_IS_NOT_FILE" -- indicates the argument is the code to be parsed (in ParseYaml mode)

main :: IO ()
main = do
    options <- readOptions
    case (iMode options) of
        ParseYaml -> do
            (contentName, content) <- getContent options
            showBreakdown <- getShowBreakdown
            let cfg = config {ncBreakdown = showBreakdown}
            let (output, _) = normalizeWithConfig cfg (yaml contentName content False)
            putStrLn output
        EncodePlain -> convertArgToScalarToken options

-- Environment variable BRKDWN indicates we should run in breakdown mode, showing
-- which token triggered which output
getShowBreakdown :: IO Bool
getShowBreakdown  = do
    evv <- lookupEnv "BRKDWN"
    return (envFlagValue evv)

envFlagValue :: Maybe String -> Bool
envFlagValue (Just "")  = False
envFlagValue Nothing    = False
envFlagValue _          = True

getContent :: Invocation -> IO (String, BS.ByteString)
getContent options@Invocation{iMainArg = (Just input)} = do
    let inputIsYaml = iImmediate options
    if inputIsYaml
        then return ("<immediate input>", BSU.fromString input)
        else do
            content <- BS.readFile input
            return (input, content)

convertArgToScalarToken :: Invocation -> IO ()
convertArgToScalarToken Invocation{iMainArg = (Just strval)} = do
    putStrLn $ scalarTokenForString strval

readOptions :: IO Invocation
readOptions  = do
    argv <- getArgs
    env <- readEnvvars [krun_is_not_file]
    return $ buildOptions argv env $ Invocation
        { iMode = ParseYaml
        , iMainArg = Nothing
        , iImmediate = False
        }

buildOptions :: [String] -> M.Map String (Maybe String) -> Invocation -> Invocation
buildOptions ("--encode":arg:args) env options = buildOptions (arg:args) env options
    { iMode = EncodePlain
    }
buildOptions [arg] env options@Invocation{ iMainArg = Nothing } = buildOptions [] env options
    { iMainArg = Just arg }
buildOptions [] env options@Invocation{ iMode = ParseYaml } = options
    { iImmediate = envFlagValue $ M.findWithDefault Nothing krun_is_not_file env
    }
buildOptions [] env options = options

readEnvvars :: [String] -> IO (M.Map String (Maybe String))
readEnvvars = accumEnv M.empty
    where
        accumEnv :: M.Map String (Maybe String) -> [String] -> IO (M.Map String (Maybe String))
        accumEnv env [] = return env
        accumEnv env (var:vars) = do
            val <- lookupEnv var
            let env' = M.insert var val env
            accumEnv env' vars
