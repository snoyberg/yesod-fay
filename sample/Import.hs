{-# LANGUAGE CPP #-}
module Import where

import Yesod.Fay
import Language.Haskell.TH.Syntax (Exp)
import System.Process (readProcess)

-- | In a standard scaffolded site, @development@ is provided by
-- @Settings.Development@.
development :: Bool
#ifdef PRODUCTION
development = False
#else
development = True
#endif

fayFile' :: Exp -> FayFile
fayFile' staticR moduleName
    | development = fayFileReload settings
    | otherwise   = fayFileProd settings
  where
    settings = (yesodFaySettings moduleName)
        { yfsSeparateRuntime = Just ("static", staticR)
        , yfsPostProcess = readProcess "java" ["-jar", "closure-compiler.jar"]
        }
