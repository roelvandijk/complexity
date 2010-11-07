{-# LANGUAGE DeriveDataTypeable
           , NoImplicitPrelude
           , UnicodeSyntax 
  #-}

{-
Lot's of code in this module was 'borrowed' from criterion.
-}

module Test.Complexity.Main where

import Test.Complexity


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Monad         ( (>>=), (=<<), (>>), return, fail, sequence )
import Data.Bool             ( otherwise )
import Data.Char             ( String )
import Data.Eq               ( Eq )
import Data.Function         ( ($), on )
import Data.Functor          ( fmap )
import Data.List             ( (++) )
import Data.Maybe            ( Maybe(Nothing, Just) )
import Data.Monoid           ( Monoid(..), Last(..) )
import Data.Ord              ( Ord, (>) )
import Data.Typeable         ( Typeable )
import Prelude               ( Bounded, Enum, Integer, fromInteger )
import System.Environment    ( getArgs, getProgName )
import System.Exit           ( ExitCode(ExitSuccess, ExitFailure), exitWith )
import System.Console.GetOpt ( ArgOrder(Permute)
                             , ArgDescr(NoArg, ReqArg)
                             , OptDescr(Option)
                             , getOpt 
                             )
import System.IO             ( IO, putStrLn )
import Text.Printf           ( printf )
import Text.Read             ( Read, reads )
import Text.Show             ( Show )

-- from base-unicode-symbols:
import Data.Function.Unicode ( (∘) ) 
import Data.Monoid.Unicode   ( (∅), (⊕) )


--------------------------------------------------------------------------------
-- Configurations
--------------------------------------------------------------------------------


data Verbosity = Quiet
               | Normal
               | Verbose
                 deriving (Eq, Ord, Bounded, Enum, Read, Show, Typeable)

data Config = Config { cfgVerbosity ∷ Last Verbosity
                     , cfgDataFile  ∷ Last String
                     , cfgTimeout   ∷ Last Integer
                     } deriving (Eq, Read, Show, Typeable)

instance Monoid Config where
    mempty  = emptyConfig
    mappend = appendConfig

ljust ∷ α → Last α
ljust = Last ∘ Just

emptyConfig ∷ Config
emptyConfig = Config { cfgVerbosity = (∅)
                     , cfgDataFile  = (∅)
                     , cfgTimeout   = (∅)
                     }

defaultConfig ∷ Config
defaultConfig = Config { cfgVerbosity = ljust Normal
                       , cfgDataFile  = (∅)
                       , cfgTimeout   = ljust 10
                       }

appendConfig ∷ Config → Config → Config
appendConfig x y = Config { cfgVerbosity = app cfgVerbosity x y
                          , cfgDataFile  = app cfgDataFile  x y
                          , cfgTimeout   = app cfgTimeout   x y
                          }
    where app f = (⊕) `on` f


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
  ]

parseArgs ∷ Config → [OptDescr (IO Config)] → [String] → IO (Config, [String])
parseArgs defCfg options args = 
    case getOpt Permute options args of
      (_, _, err:_)    → putStrLn "Er ging iets fout" >> exitWith (ExitFailure 1)
      (opts, rest, []) → do
        cfg ← ((defCfg ⊕) ∘ mconcat) `fmap` sequence opts
        return (cfg, rest)


--------------------------------------------------------------------------------
-- Default main functions
--------------------------------------------------------------------------------


defaultMain ∷ [Experiment] → IO ()
defaultMain = defaultMainWith defaultConfig

defaultMainWith ∷ Config → [Experiment] → IO ()
defaultMainWith defCfg xs = do (cfg, args) ← parseArgs defCfg defaultOptions =<< getArgs
                               putStrLn "Now I should probably do something..."
                               putStrLn "But I won't."
                               
