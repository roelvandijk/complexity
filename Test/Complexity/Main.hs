{-# LANGUAGE DeriveDataTypeable
           , NoImplicitPrelude
           , UnicodeSyntax 
  #-}

{-
Lot's of code in this module was 'borrowed' from criterion.
-}

module Test.Complexity.Main where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Monad         ( (>>=), (=<<), (>>), return, fail, sequence )
import Data.Bool             ( otherwise )
import Data.Char             ( String )
import Data.Function         ( ($) )
import Data.Functor          ( fmap )
import Data.List             ( (++) )
import Data.Maybe            ( Maybe(Nothing, Just) )
import Data.Monoid           ( Monoid(..), Last(..) )
import Data.Ord              ( Ord, (>) )
import Prelude               ( Integer, fromInteger )
import System.Environment    ( getArgs, getProgName )
import System.Exit           ( ExitCode(ExitSuccess, ExitFailure), exitWith )
import System.Console.GetOpt ( ArgOrder(Permute)
                             , ArgDescr(NoArg, ReqArg)
                             , OptDescr(Option)
                             , getOpt, usageInfo
                             )
import System.IO             ( IO, putStr, putStrLn )
import Text.Printf           ( printf )
import Text.Read             ( reads )
import Text.Show             ( Show, show )

-- from base-unicode-symbols:
import Data.Function.Unicode ( (∘) ) 
import Data.Monoid.Unicode   ( (⊕) )

-- from complexity:
import Test.Complexity.Config     ( Config(..), Exit(..), Verbosity(..)
                                  , defaultConfig, ljust 
                                  )
import Test.Complexity.Experiment ( Experiment )
import Test.Complexity.Monad      ( runConfigT )


-- from transformers:
import Control.Monad.IO.Class ( liftIO )

--------------------------------------------------------------------------------
-- Option parsing
--------------------------------------------------------------------------------

parseError ∷ String → IO α
parseError msg = do
  _ ← printf "Error: %s\n" msg
  _ ← printf "Run \"%s --help\" for usage information\n" =<< getProgName
  exitWith (ExitFailure 1)

parsePos ∷ String → String → IO Integer
parsePos q s = case reads s of
                 [(n, "")] | n > 0     → return n
                           | otherwise → parseError $ q ++ " must be positive" 
                 _                     → parseError $ "invalid " ++ q ++ " provided"

defaultOptions ∷ [OptDescr (IO Config)]
defaultOptions = 
  [ Option ['v'] 
           ["verbose"] 
           (NoArg $ return $ mempty { cfgVerbosity = ljust Verbose })
           "print more output"
  , Option ['f']
           ["file"]
           (ReqArg (\s → return $ mempty { cfgDataFile = ljust s }) "FILENAME")
           "file to store measurement results"
  , Option ['t']
           ["timeout"]
           (ReqArg (\s → parsePos "timeout" s >>= \t → return $ mempty { cfgTimeout = ljust t }) "TIMEOUT")
           ""
  , Option ['h']
           ["help"]
           (NoArg $ return $ mempty { cfgExit = ljust ExitWithHelp })
           "shows command line usage information"
  , Option ['V']
           ["version"]
           (NoArg $ return $ mempty { cfgExit = ljust ExitWithVersion })
           ""
  ]

progUsageInfo ∷ [OptDescr (IO Config)] → IO String
progUsageInfo options = do
  p ← getProgName
  return $ usageInfo ("Usage: " ⊕ p ⊕ " [OPTIONS]") options

parseArgs ∷ Config → [OptDescr (IO Config)] → [String] → IO (Config, [String])
parseArgs defCfg options args = 
    case getOpt Permute options args of
      (_, _, _:_)      → putStrLn "TODO: better error report" >> exitWith (ExitFailure 1)
      (opts, rest, []) → do
        cfg ← ((defCfg ⊕) ∘ mconcat) `fmap` sequence opts
        case getLast $ cfgExit cfg of
             Just ExitWithHelp    → do putStr =<< progUsageInfo defaultOptions
                                       exitWith ExitSuccess
             Just ExitWithVersion → do putStrLn "TODO: report version"
                                       exitWith ExitSuccess
             Nothing              → return ()
        return (cfg, rest)


--------------------------------------------------------------------------------
-- Default main functions
--------------------------------------------------------------------------------


defaultMain ∷ [Experiment] → IO ()
defaultMain = defaultMainWith defaultConfig

defaultMainWith ∷ Config → [Experiment] → IO ()
defaultMainWith defCfg experiments = do 
  (cfg, _) ← parseArgs defCfg defaultOptions =<< getArgs
  runConfigT cfg $ do
    liftIO $ putStrLn "Arguments parsed:"
    liftIO $ putStrLn $ show cfg
                               
