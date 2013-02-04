{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
module SharedTypes where

import Language.Fay.Yesod
import Data.Data
#ifdef FAY
import FFI
#else
import Language.Fay.FFI
#endif

data Command = RollDie (Returns Text)
    deriving (Read, Typeable, Data)
instance Foreign Command
