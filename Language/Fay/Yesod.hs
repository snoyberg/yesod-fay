{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude  #-}
-- | Module to be shared between server and client.
--
-- This module must be valid for both GHC and Fay.
module Language.Fay.Yesod where

import           Language.Fay.FFI
import           Language.Fay.Prelude

-- | A proxy type for specifying what type a command should return. The final
-- field for each data constructor in a command datatype should be @Returns@.
data Returns a = Returns
    deriving (Show, Read, Data, Typeable)

-- | Call a command.
call :: (Foreign a, Foreign command)
     => (Returns a -> command)
     -> (a -> Fay ())
     -> Fay ()
call f g = ajaxCommand (f Returns) g

-- | Run the AJAX command.
ajaxCommand :: (Foreign a, Foreign command)
            => command
            -> (a -> Fay ())
            -> Fay ()
ajaxCommand = ffi "jQuery['ajax']({\
                  \ \"url\": window['yesodFayCommandPath'], \
                  \ \"type\": 'POST', \
                  \ \"data\": { \"json\": JSON.stringify(%1) }, \
                  \ \"dataType\": 'json', \
                  \ \"success\" : %2 \
                  \})"
