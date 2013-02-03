{-# LANGUAGE CPP #-}
module Import where

import Yesod.Fay

-- | In a standard scaffolded site, @development@ is provided by
-- @Settings.Development@.
development :: Bool
#ifdef PRODUCTION
development = False
#else
development = True
#endif

fayFile :: FayFile
fayFile moduleName
    | development = fayFileReload settings
    | otherwise   = fayFileProd settings
  where
    settings = yesodFaySettings moduleName
