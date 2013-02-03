{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
module SharedTypes where

import Language.Fay.Prelude
import Language.Fay.Yesod
import Language.Fay.FFI

data Command = RollDie (Returns Text)
    deriving (Read, Typeable, Data)
instance Foreign Command
