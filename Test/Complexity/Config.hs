
{-# LANGUAGE DeriveDataTypeable
           , NoImplicitPrelude
           , UnicodeSyntax 
  #-}

{-
Lot's of code in this module was 'borrowed' from criterion.
-}

module Test.Complexity.Config where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Monad         ( (>>=), fail )
import Data.Char             ( String )
import Data.Eq               ( Eq )
import Data.Function         ( on )
import Data.Maybe            ( Maybe(Just) )
import Data.Monoid           ( Monoid(..), Last(..) )
import Data.Ord              ( Ord )
import Data.Typeable         ( Typeable )
import Prelude               ( Bounded, Enum, Integer, fromInteger )
import Text.Read             ( Read )
import Text.Show             ( Show )

-- from base-unicode-symbols:
import Data.Function.Unicode ( (∘) ) 

--------------------------------------------------------------------------------
-- Configurations
--------------------------------------------------------------------------------


data Verbosity = Quiet
               | Normal
               | Verbose
                 deriving (Eq, Ord, Bounded, Enum, Read, Show, Typeable)

data Exit = ExitWithVersion
          | ExitWithHelp
            deriving (Eq, Ord, Bounded, Enum, Read, Show, Typeable)

data Config = Config { cfgVerbosity ∷ Last Verbosity
                     , cfgDataFile  ∷ Last String
                     , cfgTimeout   ∷ Last Integer
                     , cfgExit      ∷ Last Exit
                     } deriving (Eq, Read, Show, Typeable)

instance Monoid Config where
    mempty  = emptyConfig
    mappend = appendConfig

ljust ∷ α → Last α
ljust = Last ∘ Just

emptyConfig ∷ Config
emptyConfig = Config { cfgVerbosity = mempty
                     , cfgDataFile  = mempty
                     , cfgTimeout   = mempty
                     , cfgExit      = mempty
                     }

defaultConfig ∷ Config
defaultConfig = Config { cfgVerbosity = ljust Normal
                       , cfgDataFile  = mempty
                       , cfgTimeout   = ljust 10
                       , cfgExit      = mempty
                       }

appendConfig ∷ Config → Config → Config
appendConfig x y = Config { cfgVerbosity = app cfgVerbosity x y
                          , cfgDataFile  = app cfgDataFile  x y
                          , cfgTimeout   = app cfgTimeout   x y
                          , cfgExit      = app cfgExit      x y
                          }
    where app f = mappend `on` f

