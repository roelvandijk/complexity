{-# LANGUAGE GeneralizedNewtypeDeriving
           , NoImplicitPrelude
           , UnicodeSyntax 
  #-}

module Test.Complexity.Monad where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Applicative        ( Applicative )
import Control.Monad              ( Monad )
import Data.Functor               ( Functor )

-- from complexity:
import Test.Complexity.Config     ( Config )

-- from transformers:
import Control.Monad.IO.Class     ( MonadIO )
import Control.Monad.Trans.Reader ( ReaderT, runReaderT, ask )
import Control.Monad.Trans.Class  ( MonadTrans )


--------------------------------------------------------------------------------
-- ConfigT, an environment where you can access the configuration
--------------------------------------------------------------------------------


newtype ConfigT m α = ConfigT {unConfigT ∷ ReaderT Config m α}
                        deriving ( Functor
                                 , Applicative
                                 , Monad
                                 , MonadTrans
                                 , MonadIO
                                 )

getConfig ∷ Monad m ⇒ ConfigT m Config
getConfig = ConfigT ask

runConfigT ∷ Config → ConfigT m α → m α
runConfigT c cT = runReaderT (unConfigT cT) c

