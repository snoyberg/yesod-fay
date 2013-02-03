{-# LANGUAGE OverloadedStrings #-}
module Yesod.Fay.Internal
    ( removeCPP
    ) where

import qualified Data.Text as T

removeCPP :: T.Text -> T.Text
removeCPP =
    T.unlines . start . T.lines
  where
    start [] = []
    start ("#ifdef FAY":rest) = fayOnly rest
    start (x:xs) = x : start xs

    fayOnly [] = []
    fayOnly ("#else":rest) = ghcOnly rest
    fayOnly (x:xs) = x : fayOnly xs

    ghcOnly [] = []
    ghcOnly ("#endif":rest) = start rest
    ghcOnly (_:xs) = ghcOnly xs
