module Home where

import Language.Fay.Yesod
import Language.Fay.JQuery
import SharedTypes
import FFI
import Prelude

main :: Fay ()
main = void $ select "#roll" >>= onClick (const $ call RollDie alert >> return False)
