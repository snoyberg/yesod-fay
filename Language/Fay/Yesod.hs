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
     -> (a -> Fay ()) -- ^ Success Handler
     -> Fay ()
call f g = ajaxCommand (f Returns) g

-- ! Call a command, handling errors as well
callWithErrorHandling :: (Foreign a, Foreign command)
     => (Returns a -> command)
     -> (a -> Fay ()) -- ^ Success Handler
     -> (Fay ())      -- ^ Failure Handler
     -> Fay ()
callWithErrorHandling f g h = ajaxCommandWithErrorHandling (f Returns) g h

-- | Run the AJAX command.
ajaxCommand :: (Foreign a, Foreign command)
            => Automatic command
            -> (a -> Fay ()) -- ^ Success Handler
            -> Fay ()
ajaxCommand = ffi "jQuery['ajax']({ url: window['yesodFayCommandPath'], type: 'POST', data: { json: JSON.stringify(%1) }, dataType: 'json', success : %2})"

-- | Run the AJAX command, handling errors as well
ajaxCommandWithErrorHandling :: (Foreign a, Foreign command)
            => Automatic command
            -> (a -> Fay ()) -- ^ Success Handler
            -> (Fay ())      -- ^ Failure Handler
            -> Fay ()
ajaxCommandWithErrorHandling = ffi "jQuery['ajax']({ url: window['yesodFayCommandPath'], type: 'POST', data: { json: JSON.stringify(%1) }, dataType: 'json', success : %2, error: %3})"

