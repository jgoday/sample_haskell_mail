-- Modulo para opcions usando getOpt
module Args(
    argsGet,
    argsCreateRequiredOption,
    argsCreateOptionalOption,
    argsCreateNoargOption
) where

import Control.Monad

import Data.Typeable

import System.Console.GetOpt
import System.Environment
import System.Exit


-- Devolve os argumentos e o erro
argsGet opts = do
    (args, _, err) <- getArgs >>= return . getOpt RequireOrder opts
    _args_check opts args err

    return args

_args_check opts args err = do
    when (not . null $ err) $ do { mapM_ putStrLn err;
                                 exitWith $ ExitFailure 1 }
    when (null args) $ do { putStrLn (usageInfo "" opts);
                                 exitWith ExitSuccess }

argsCreateRequiredOption s t d o=
    Option[s][t] (ReqArg d t) o
argsCreateOptionalOption s t d o=
    Option[s][t] (OptArg d t) o
argsCreateNoargOption s t d o =
    Option[s][t] (NoArg d) o

