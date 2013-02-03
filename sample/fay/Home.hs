{-# LANGUAGE NoImplicitPrelude #-}
module Home where

import Language.Fay.Prelude
import Language.Fay.Yesod
import Language.Fay.JQuery
import SharedTypes

main :: Fay ()
main = void $ select "#roll" >>= onClick (const $ call RollDie alert >> return False)
